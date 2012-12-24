{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts,
             GeneralizedNewtypeDeriving #-}
module Main (main) where

import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Monad (when)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Directory (getAppUserDataDirectory)
import Control.Monad.State (StateT)
import Control.Monad.State.Class (MonadState(get, put))
import Control.Applicative ((<$>), (<*>), pure, optional)
import Options.Applicative
    (Parser, argument, metavar, long, short, strOption, help, value,
     subparser, command, info, progDesc, execParser, helper, fullDesc, header)

data Book = Book { bookAuthor :: Text
                 , bookTitle  :: Text }
  deriving (Show, Eq)

data BookList = BookList { blReading  :: [Book]
                         , blToBeRead :: [Book] }
  deriving (Show, Eq)

class (Functor m, Monad m, MonadState BookList m, MonadIO m) => MonadBooks m


newtype BooksM a = BM { runBM :: StateT BookList IO a }
    deriving (Functor, Monad, MonadState BookList, MonadIO)

instance MonadBooks BooksM

runBooks :: BooksM a -> IO a
runBooks = undefined -- TODO use a proper monad

-- | Query the given list of books for a book that matches the given search
-- criteria.
query :: [Book] -> Text -> [Book]
query = undefined -- TODO

putLn :: MonadIO m => Text -> m ()
putLn = liftIO . TIO.putStrLn

show_ :: Show a => a -> Text
show_ = T.pack . show

status :: MonadBooks m => m ()
status = do
    BookList{..} <- get
    when (length blReading > 0) $ do
        putLn "Reading:"
        mapM_ (putLn . formatBookNice) blReading
    putLn $ "There are " <> show_ (length blToBeRead)
                         <> " books to be read."

formatBookNice :: Book -> Text
formatBookNice Book{..} = bookTitle <> " by " <> bookAuthor

finishNoQuery :: MonadBooks m => m ()
finishNoQuery = do
    bl@(BookList{..}) <- get
    case blReading of
        [] -> putLn "You are not reading any book at the moment."
        [b] -> do
            put (bl { blReading = [] })
            putLn $ "Finished reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. You are reading:"
            mapM_ (putLn . formatBookNice) bs

finishWithQuery :: MonadBooks m => Text -> m ()
finishWithQuery q = do -- TODO abstract away query checking
    bl@(BookList{..}) <- get
    case (query blReading q) of
        [] -> putLn "Could not find such a book."
        [b] -> do
            put (bl { blReading = [] })
            putLn $ "Finished reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. That query matches:"
            mapM_ (putLn . formatBookNice) bs

list :: MonadBooks m => m ()
list = do
    BookList{..} <- get

    let toBeRead = zip nats blToBeRead
        reading  = zip nats blReading

    when (length reading > 0) $ do
        putLn "Reading:"
        mapM_ (putLn . format) reading
        putLn ""

    putLn "To be read:"
    mapM_ (putLn . format) toBeRead

  where nats = 1:map (+1) nats

        format :: (Integer, Book) -> Text
        format (i, Book{..}) = show_ i <> ". "  <> bookAuthor
                                      <> " - " <> bookTitle

pick :: MonadBooks m => Text -> m ()
pick q = do
    bl@(BookList{..}) <- get
    case query blToBeRead q of
        [] -> putLn "Could not find such a book."
        [b] -> do
            put $ bl { blReading  = b:blReading
                     , blToBeRead = filter (/= b) blToBeRead  }
            putLn $ "Finished reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. That query matches:"
            mapM_ (putLn . formatBookNice) bs

add :: MonadBooks m => Text -> Text -> m ()
add title author = do
    bl@(BookList{blToBeRead = toBeRead }) <- get -- TODO check dupes
    put $ bl { blToBeRead = book:toBeRead }
    putLn $ "Added " <> formatBookNice book <> " to the reading list."
  where book = Book { bookTitle = title, bookAuthor = author }

remove :: MonadBooks m => Text -> m ()
remove q = do
    bl@(BookList{blToBeRead = toBeRead}) <- get
    case query toBeRead q of
        [] -> putLn "Could not find such a book."
        [b] -> do
            put $ bl { blToBeRead = filter (/= b) toBeRead  }
            putLn $ "Removed " <> formatBookNice b <> " from the reading list."
        bs -> do
            putLn "Can you be more specific. That query matches:"
            mapM_ (putLn . formatBookNice) bs

stopNoQuery :: MonadBooks m => m ()
stopNoQuery = do
    bl@(BookList{..}) <- get
    case blReading of
        [] -> putLn "You are not reading any book."
        [b] -> do
            put $ bl { blReading = filter (/= b) blReading
                     , blToBeRead = b:blToBeRead }
            putLn $ "Stopped reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. You are reading:"
            mapM_ (putLn . formatBookNice) bs

stopWithQuery :: MonadBooks m => Text -> m ()
stopWithQuery q = do
    bl@(BookList{..}) <- get
    case query blReading q of
        [] -> putLn "Could not find such a book."
        [b] -> do
            put $ bl { blReading = filter (/= b) blReading
                     , blToBeRead = b:blToBeRead }
            putLn $ "Stopped reading " <> formatBookNice b
        bs -> do
            putLn "Can you be more specific. That query matches:"
            mapM_ (putLn . formatBookNice) bs

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
dispatch args = do
  putStrLn $ unwords ["Using file", argFile args]
  case argCommand args of
    Add{..}    -> runBooks $ add addTitle addAuthor
    Finish{..} -> runBooks $ maybe finishNoQuery finishWithQuery finishQuery
    List       -> runBooks   list
    Pick{..}   -> runBooks $ pick pickQuery
    Remove{..} -> runBooks $ remove removeQuery
    Status     -> runBooks   status
    Stop{..}   -> runBooks $ maybe stopNoQuery stopWithQuery stopQuery
        

        
{-
  putStrLn $ unwords ["Using file", argFile args]
  case argCommand args of
    Add{..} ->
        putStrLn $ unwords ["Adding", addTitle, "by", addAuthor]
    Remove{..} ->
        putStrLn $ unwords ["Removing", removeTitle, "by", removeAuthor]
    List ->
        putStrLn "Listing all books."
    Pick ->
        putStrLn "Picking a book at random."
-}

-- | Represents the subcommands offered by the program.
data Command = Add { addTitle :: Text
                   , addAuthor  :: Text }
             | Finish { finishQuery :: Maybe Text }
             | List
             | Pick   { pickQuery   :: Text }
             | Remove { removeQuery :: Text }
             | Status 
             | Stop   { stopQuery   :: Maybe Text }

-- | Represents all the command line arguments accepted by the program.
data Argument = Argument { argFile    :: FilePath
                         , argCommand :: Command
                         }

addParser :: Parser Command
addParser = Add <$> argument str (metavar "TITLE")
                <*> argument str (metavar "AUTHOR")

finishParser :: Parser Command
finishParser = Finish <$> optional (argument str (metavar "QUERY"))

stopParser :: Parser Command
stopParser   = Stop   <$> optional (argument str (metavar "QUERY"))

pickParser :: Parser Command
pickParser   = Pick   <$> argument str (metavar "QUERY")

removeParser :: Parser Command
removeParser = Remove <$> argument str (metavar "QUERY")

-- | Parser for all subcommands.
commandParser :: Parser Command
commandParser = subparser $
    command "add"    (info addParser     $ progDesc "Add a book.")
 <> command "finish" (info finishParser  $ progDesc "Mark a book finished.")
 <> command "list"   (info (pure List)   $ progDesc "List all books to be read.")
 <> command "pick"   (info pickParser    $ progDesc "Start reading a book.")
 <> command "remove" (info removeParser  $ progDesc "Remove a book.")
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
