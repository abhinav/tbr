{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleContexts,
             GeneralizedNewtypeDeriving, DeriveGeneric, TemplateHaskell,
             Rank2Types, QuasiQuotes #-}
module Main (main) where

import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Monad (when)
import Data.List (isInfixOf)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Text.Shakespeare.Text (st)
import Data.Default (Default(def))
import qualified Data.Text.IO as TIO
import Data.Char (isAlphaNum, isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.FilePath ((</>), takeDirectory)
import Control.Monad.State.Class (MonadState)
import Script (ScriptT, runScriptT, scriptIO)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Control.Applicative ((<$>), (<*>), pure, optional)
import Control.Error (note, hoistEither, catchT, right, left)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import Control.Lens (makeLenses, (<>=), use, (%=), uses, (^.), views, Lens')
import Options.Applicative
    (Parser, argument, metavar, long, short, strOption, help, value,
     subparser, command, info, progDesc, execParser, helper, fullDesc, header)

-- | Represents a Book.
data Book = Book { _bookAuthor :: Text
                 , _bookTitle  :: Text }
  deriving (Show, Eq, Generic)
makeLenses ''Book
instance ToJSON Book
instance FromJSON Book

-- | Represents the complete reading list. Contains both, the books that are
-- to be read and that are being read.
data BookList = BookList { _blReading  :: [Book]
                         , _blToBeRead :: [Book] }
  deriving (Show, Eq, Generic)
makeLenses ''BookList
instance ToJSON BookList
instance FromJSON BookList

instance Default BookList where
    def = BookList def def

-- | The BooksM monad allows access to the @BookList@.
newtype BooksM a = BM { runBM :: ScriptT (StateT BookList IO) a }
  deriving (Monad, MonadState BookList, MonadIO)

-- | Converts the given strict bytestring into a lazy bytestring.
toLazy :: BS.ByteString -> BSL.ByteString
toLazy bs = BSL.fromChunks [bs]

-- | Converts the given lazy bytestring into a strict bytestring.
fromLazy :: BSL.ByteString -> BS.ByteString
fromLazy = BS.concat . BSL.toChunks

-- | Execute the @BooksM@ monad.
runBooksM :: FilePath -> BooksM a -> IO a
runBooksM path b = do
    -- Attempt to read the contents of the file into a BookList. If the
    -- operation fails for any reason, use a default BookList.
    bookList <- runScriptT $ catchT getList
                                    (const $ right def)
    (a, bookList') <- runStateT (runScriptT $ runBM b) bookList
    -- If the BookList has been changed, write it the file.
    when (bookList /= bookList') $ runScriptT $ putList bookList'
    return a
  where
    getList = do
        contents <- toLazy <$> (scriptIO $ BS.readFile path)
        hoistEither $ note "The book list is in an invalid format."
                           (decode contents)
    putList bl = scriptIO $ do
        createDirectoryIfMissing True (takeDirectory path)
        BS.writeFile path (fromLazy $ encode bl)

-- | Query the given list of books for a book that matches the given search
-- criteria.
query' :: [Book] -> Text -> [Book]
query' bs q = filter matchBook bs
  where clean   t = T.toLower (T.filter isValid t)
        isValid   = (||) <$> isSpace <*> isAlphaNum
        split   s = T.split isSpace s
        qTokens   = split (clean q)
        matches s = qTokens `isInfixOf` split (clean s)
        matchBook = (||) <$> views bookTitle  matches
                         <*> views bookAuthor matches

-- | @query'@ that runs inside a Monad that contains the BookList. The first
-- argument is a lens over the BookList that chooses which list of books
-- (to-be-read or reading) is to be queried.
query :: (MonadState BookList m) => Lens' BookList [Book] -> Text -> m [Book]
query l q = uses l (`query'` q)

-- | Similar to @query@ except that only one result is expected. If the query
-- results in no books, or more than one books, an error is thrown.
queryOne :: Lens' BookList [Book] -> Text -> BooksM Book
queryOne l q = BM $ do
    bs <- query l q
    case bs of
        []  -> left ("Unable to find a book matching the query: " <> T.unpack q)
        [b] -> return b
        _   -> do putLn "The query matched:"
                  printBookList bs
                  left "Please refine the query."

-- | Get the book currently being read. If the reading list is empty, or there
-- are greater than one books in it, then an error is thrown.
getReading :: BooksM Book
getReading = BM $ do
    bs <- use blReading
    case bs of
        []  -> left "You are not reading any book at the moment."
        [b] -> return b
        _   -> do putLn "You are currently reading:"
                  printBookList bs
                  left "Please provide a query."

-- | A version of @Data.Text.IO.putStrLn@ that runs in any monad capable of
-- lifting IO operations.
putLn :: MonadIO m => Text -> m ()
putLn = liftIO . TIO.putStrLn

-- START COMMANDS:

status :: BooksM ()
status = do
    putLn "Reading:"
    use blReading >>= printBookList

    tbrCount <- uses blToBeRead length
    putLn [st|There are #{show tbrCount} books to be read.|]

formatBook :: Book -> Text
formatBook b = [st|#{title} by #{author}|]
    where title  = b^.bookTitle
          author = b^.bookAuthor

finish :: Book -> BooksM ()
finish b = do blReading %= filter (/= b)
              putLn [st|Finished reading #{formatBook b}|]

search :: Text -> BooksM ()
search q = do
    reading  <- query blReading  q
    toBeRead <- query blToBeRead q

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

list :: BooksM ()
list = do
    reading <- use blReading
    toBeRead <- use blToBeRead

    when (length reading > 0) $ do
        putLn "Reading:"
        printBookList reading
        when (length toBeRead > 0) (putLn "")

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
        format (i, b) = [st|#{show i}. #{extraSpaces <> author} - #{title}|]
          where extraSpaces = T.replicate (maxNumLength - numLength i) " "
                author = b ^. bookAuthor
                title  = b ^. bookTitle

-- TODO fix the following error:
-- tbr: user error (/Users/abhinav/.tbr/tbr.txt: openBinaryFile: resource busy
-- (file is locked))
pick :: Text -> BooksM ()
pick q = do
    b <- queryOne blToBeRead q
    blReading <>= [b]
    blToBeRead %= filter (/= b)
    putLn [st|Started reading #{formatBook b}.|]

add :: Text -> Text -> BooksM ()
add title author = do
    blToBeRead <>= [book]
    putLn [st|Added #{formatBook book} to the reading list.|]
  where book = Book author title

remove :: Text -> BooksM ()
remove q = do
    b <- queryOne blToBeRead q
    blToBeRead %= filter (/= b)
    putLn [st|Removed #{formatBook b} from the reading list.|]

stop :: Book -> BooksM ()
stop b = do blReading  %= filter (/= b)
            blToBeRead <>= [b]
            putLn [st|Stopped redaing #{formatBook b}.|]

-- COMMAND LINE STUFF STARTS HERE:

str :: Monad m => String -> m Text
str = return . T.pack

-- | Dispatch the correct subcommand based on the options.
dispatch :: Argument -> IO ()
dispatch args = runBooksM (argFile args) $
  case argCommand args of
    Add{..}    -> add addTitle addAuthor
    Finish{..} -> maybe getReading (queryOne blReading) finishQuery >>= finish
    List       -> list
    Pick{..}   -> pick pickQuery
    Remove{..} -> remove removeQuery
    Search{..} -> search searchQuery
    Status     -> status
    Stop{..}   -> maybe getReading (queryOne blReading) stopQuery >>= stop
        
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
