{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts,
             GeneralizedNewtypeDeriving, DeriveGeneric, MultiParamTypeClasses,
             TemplateHaskell #-}
module Main (main) where

import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.List (isInfixOf)
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isAlphaNum, isSpace)
import qualified Data.ByteString.Lazy as BSL
import System.FilePath ((</>), takeDirectory)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Control.Applicative ((<$>), (<*>), pure, optional)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import Control.Lens (makeLenses, (<>=), (.=), use, (%=), uses, (^.), views)
import Options.Applicative
    (Parser, argument, metavar, long, short, strOption, help, value,
     subparser, command, info, progDesc, execParser, helper, fullDesc, header)

data Book = Book { _bookAuthor :: Text
                 , _bookTitle  :: Text }
  deriving (Show, Eq, Generic)
instance ToJSON Book
instance FromJSON Book

makeLenses ''Book

data BookList = BookList { _blReading  :: [Book]
                         , _blToBeRead :: [Book] }
  deriving (Show, Eq, Generic)
instance ToJSON BookList
instance FromJSON BookList

makeLenses ''BookList

class (Monad m, MonadState BookList m, MonadIO m) => MonadBooks m

newtype BooksM a = BM { runBM :: StateT BookList IO a }
  deriving (Monad, MonadState BookList, MonadIO)

instance MonadBooks BooksM

-- FIXME
runBooks :: FilePath -> BooksM a -> IO a
runBooks path b = do
    bookList <- readBookList path
    (a, bookList') <- runStateT (runBM b) bookList
    when (bookList /= bookList') $
        writeBookList path bookList'
    return a

readBookList :: FilePath -> IO BookList
readBookList path = fromJust . decode <$> BSL.readFile path

writeBookList :: FilePath -> BookList -> IO ()
writeBookList path bl = do
    createDirectoryIfMissing True (takeDirectory path)
    BSL.writeFile path (encode bl)

-- | Query the given list of books for a book that matches the given search
-- criteria.
query :: [Book] -> Text -> [Book]
query bs q = filter matchBook bs
  where clean   t = T.toLower (T.filter isValid t)
        isValid   = (||) <$> isSpace <*> isAlphaNum
        split   s = T.split isSpace s
        qTokens   = split (clean q)
        matches s = qTokens `isInfixOf` split (clean s)
        matchBook = (||) <$> views bookTitle  matches
                         <*> views bookAuthor matches

putLn :: MonadIO m => Text -> m ()
putLn = liftIO . TIO.putStrLn

show_ :: Show a => a -> Text
show_ = T.pack . show

status :: MonadBooks m => m ()
status = do
    reading <- use blReading
    tbrCount <- uses blToBeRead length
    case reading of
        []  -> return ()
        [b] ->
            putLn $ "Reading " <> formatBookNice b
        bs  ->  do
            putLn "Reading:"
            printBookList bs
    putLn $ "There are " <> show_ tbrCount <> " books to be read."

formatBookNice :: Book -> Text
formatBookNice b = (b ^. bookTitle) <> " by " <> (b ^. bookAuthor)

finishNoQuery :: MonadBooks m => m ()
finishNoQuery = do
    reading <- use blReading
    case reading of
        [] -> putLn "You are not reading any book at the moment."
        [b] -> do
            blReading .= []
            putLn $ "Finished reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. You are reading:"
            printBookList bs

finishWithQuery :: MonadBooks m => Text -> m ()
finishWithQuery q = do -- TODO abstract away query checking
    reading <- use blReading
    case (query reading q) of
        [] -> putLn "Could not find such a book."
        [b] -> do
            blReading %= filter (/= b)
            putLn $ "Finished reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. That query matches:"
            printBookList bs

search :: MonadBooks m => Text -> m ()
search q = do
    reading  <- uses blReading  queryList
    toBeRead <- uses blToBeRead queryList

    let readCount = length reading
        tbrCount  = length toBeRead

    when (readCount > 0) $ do
        putLn "Reading:"
        printBookList reading

    when (tbrCount > 0) $ do
        putLn "To be read:"
        printBookList toBeRead

    when ((readCount, tbrCount) == (0, 0)) $ do
        putLn "No such books found."

  where queryList l = query l q

list :: MonadBooks m => m ()
list = do
    reading <- use blReading
    toBeRead <- use blToBeRead

    when (length reading > 0) $ do
        putLn "Reading:"
        printBookList reading
        putLn ""

    when (length toBeRead > 0) $ do
        putLn "To be read:"
        printBookList toBeRead

printBookList :: (MonadIO m) => [Book] -> m ()
printBookList bs = liftIO $ mapM_ TIO.putStrLn (formatBookList bs)

formatBookList :: [Book] -> [Text]
formatBookList bs = map format pairs
  where nats = 1:map (+1) nats
        pairs = zip nats bs
        numLength = length . show
        maxNumLength =  numLength (length bs)
        format (i, b) = show_ i <> ". " <> extraSpaces <> author
                                <> " - "  <> title
          where extraSpaces = T.replicate (maxNumLength - numLength i) " "
                author = b ^. bookAuthor
                title  = b ^. bookTitle

-- TODO fix the following error:
-- tbr: user error (/Users/abhinav/.tbr/tbr.txt: openBinaryFile: resource busy
-- (file is locked))
pick :: MonadBooks m => Text -> m ()
pick q = do
    toBeRead <- use blToBeRead
    case query toBeRead q of
        [] -> putLn "Could not find such a book."
        [b] -> do
            blReading  <>= [b]
            blToBeRead %= filter (/= b)
            putLn $ "Finished reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. That query matches:"
            printBookList bs

add :: MonadBooks m => Text -> Text -> m ()
add title author = do
    blToBeRead <>= [book]
    putLn $ "Added " <> formatBookNice book <> " to the reading list."
  where book = Book author title

remove :: MonadBooks m => Text -> m ()
remove q = do
    toBeRead <- use blToBeRead
    case query toBeRead q of
        [] -> putLn "Could not find such a book."
        [b] -> do
            blToBeRead %= filter (/= b)
            putLn $ "Removed " <> formatBookNice b <> " from the reading list."
        bs -> do
            putLn "Can you be more specific. That query matches:"
            printBookList bs

stopNoQuery :: MonadBooks m => m ()
stopNoQuery = do
    reading <- use blReading
    case reading of
        [] -> putLn "You are not reading any book."
        [b] -> do
            blReading %= filter (/= b)
            blToBeRead <>= [b]
            putLn $ "Stopped reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. You are reading:"
            printBookList bs

stopWithQuery :: MonadBooks m => Text -> m ()
stopWithQuery q = do
    reading <- use blReading
    case query reading q of
        [] -> putLn "Could not find such a book."
        [b] -> do
            blReading %= filter (/= b)
            blToBeRead <>= [b]
            putLn $ "Stopped reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. That query matches:"
            printBookList bs

-- Interaction mockup:
--
-- $ tbr status
-- Currently reading: Good Omens by Terry Pratchett; Neil Gaiman
-- There are 42 other books to be read.
-- $ tbr finish
-- Finished reading Good Omens by Terry Pratchett; Neil Gaiman
-- $ tbr status
-- There are 42 books to be read.
-- $ tbr list
-- To be read:
-- 1.  Brandon Sanderson - The Final Empire
-- 2.                    - The Way of Kings
-- 3.  Brian K. Vaughan - Y: The Last Man, Volume 3
-- 4.  Connie Willis - Blackout
-- 5.                - Doomsday Book
-- [...]
-- 42. William Gibson - Neuromancer
-- $ tbr pick 'way of kings'
-- Started reading The Way of Kings by Brandon Sanderson.
-- $ tbr add 'Foundation and Empire' 'isaac asimov'
-- Added Foundation and Empire by Isaac Asimov to the reading list.
-- $ tbr remove 'y the last man'
-- Removed Y: The Last Man, Volume 3 by Brian K. Vaughan from the reading
-- list.
-- $ tbr pick 'foundation and empire'
-- Started reading Foundation and Empire by Isaac Asimov.
-- $ tbr stop
-- Can you be more specific? You are currently reading:
-- - Brandon Sanderson - The Way of Kings
-- - Isaac Asimov - Foundation and Empire
-- $ tbr stop 'way of kings'
-- Stopped reading The Way of Kings by Brandon Sanderson.
-- $ tbr remove 'way of kings'
-- Removed The Way of Kings by Brandon Sanderson from the reading list.

-- COMMAND LINE STUFF STARTS HERE:

str :: Monad m => String -> m Text
str = return . T.pack

-- | Dispatch the correct subcommand based on the options.
dispatch :: Argument -> IO ()
dispatch args = runBooks (argFile args) $
  case argCommand args of
    Add{..}    -> add addTitle addAuthor
    Finish{..} -> maybe finishNoQuery finishWithQuery finishQuery
    List       -> list
    Pick{..}   -> pick pickQuery
    Remove{..} -> remove removeQuery
    Search{..} -> search searchQuery
    Status     -> status
    Stop{..}   -> maybe stopNoQuery stopWithQuery stopQuery
        
-- | Represents the subcommands offered by the program.
data Command = Add { addTitle :: Text
                   , addAuthor  :: Text }
             | Finish { finishQuery :: Maybe Text }
             | List
             | Pick   { pickQuery   :: Text }
             | Remove { removeQuery :: Text }
             | Search { searchQuery :: Text }
             | Status 
             | Stop   { stopQuery   :: Maybe Text }

-- | Represents all the command line arguments accepted by the program.
data Argument = Argument { argFile    :: FilePath
                         , argCommand :: Command
                         }

addParser, finishParser, stopParser, pickParser, removeParser, searchParser
    :: Parser Command

addParser    = Add    <$> argument str (metavar "TITLE")
                      <*> argument str (metavar "AUTHOR")
finishParser = Finish <$> optional (argument str (metavar "QUERY"))
stopParser   = Stop   <$> optional (argument str (metavar "QUERY"))
pickParser   = Pick   <$> argument str (metavar "QUERY")
removeParser = Remove <$> argument str (metavar "QUERY")
searchParser = Search <$> argument str (metavar "QUERY")

-- | Parser for all subcommands.
commandParser :: Parser Command
commandParser = subparser $
    command "add"    (info addParser     $ progDesc "Add a book.")
 <> command "finish" (info finishParser  $ progDesc "Mark a book finished.")
 <> command "list"   (info (pure List)   $ progDesc "List all books to be read.")
 <> command "pick"   (info pickParser    $ progDesc "Start reading a book.")
 <> command "remove" (info removeParser  $ progDesc "Remove a book.")
 <> command "search" (info searchParser  $ progDesc "Search for a book in the list.")
 <> command "status" (info (pure Status) $ progDesc "Show reading status.")
 <> command "stop"   (info stopParser    $ progDesc "Stop reading a book.")

-- | Build the complete command line argument parser.
getArgumentParser :: IO (Parser Argument)
getArgumentParser = do
    appDir <- getAppUserDataDirectory "tbr"
    return $ Argument
                <$> strOption ( long "file"
                             <> short 'f'
                             <> metavar "FILE"
                             <> help "The reading list."
                             <> value (appDir </> "tbr.txt"))
                <*> commandParser

main :: IO ()
main = do
    parser <- getArgumentParser
    dispatch =<< execParser (info (helper <*> parser)
                                  ( fullDesc
                                 <> header "A tool to maintain reading lists."))
