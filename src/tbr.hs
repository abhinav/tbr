{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving,
             QuasiQuotes, FlexibleContexts #-}
module Main (main) where

import qualified TBR.Types              as TBR
import           TBR.Script
import           TBR.Reader
import           TBR.Writer

import           Data.Text              (Text)
import           Data.List              (isInfixOf, sortBy, groupBy,
                                         intersect)
import           Data.Char              (isAlphaNum, isSpace)
import qualified Data.Text              as T
import           Data.Default
import qualified Data.Text.IO           as TIO
import           Control.Error          (hoistEither, catchT, right, left)
import           Control.Monad          (when, unless)
import           System.FilePath        ((</>), takeDirectory)
import           System.Directory
import qualified Data.Text.Lazy.IO      as TLIO
import           Options.Applicative
import           Control.Monad.State
import           Text.Shakespeare.Text  (st)
import           Control.Monad.IO.Class

data Book = Book { bookAuthor :: Text
                 , bookTitle  :: Text }
    deriving (Show, Eq)

data BookList = BookList { blReading     :: [Book]
                         , blToBeRead    :: [Book]
                         , blLowPriority :: [Book] }
    deriving (Show, Eq)

instance Default BookList where
    def = BookList [] [] []

--------------------------------------------------------------------------------
-- Data type conversion

-- | Convert a @Document@ into a @BookList@. Throws an error if the three
-- sections are absent.
documentToBookList :: (Monad m, Functor m) => TBR.Document -> ScriptT m BookList
documentToBookList TBR.Document{..} = BookList
                                  <$> blockBooks "Reading"
                                  <*> blockBooks "To be read"
                                  <*> blockBooks "Low priority"
  where
    blockBooks :: (Monad m, Functor m) => Text -> ScriptT m [Book]
    blockBooks name = concatMap entryToBooks <$> lookupBlock name

    -- Get the entries for the block with the given name or fail.
    lookupBlock :: (Monad m, Functor m) => Text -> ScriptT m [TBR.Entry]
    lookupBlock name = 
        case filter (match . TBR.blockHeader) documentBlocks of
            []  -> left [st|Could not find the section: #{name}|]
            [x] -> right (TBR.blockEntries x)
            _   -> left [st|Multiple "#{name}" sections found.|]
        where nameTok = tokens name
              match   = (== nameTok) . tokens

    entryToBooks :: TBR.Entry -> [Book]
    entryToBooks TBR.Entry{..} = map (Book entryAuthor) entryBooks

-- | Convert a @BookList@ into a @Document@.
bookListToDocument :: BookList -> TBR.Document
bookListToDocument BookList{..} = TBR.Document
    [ toBlock "Reading"      blReading
    , toBlock "To Be Read"   blToBeRead
    , toBlock "Low Priority" blLowPriority ]
  where
    bookCompare a b    = compare (bookAuthor a) (bookAuthor b)
    bookGroup   a b    = bookAuthor a == bookAuthor b
    toBlock name books = let sorted = sortBy bookCompare books
                             grouped = groupBy bookGroup sorted
                             entries = map toEntry grouped
                         in TBR.Block name entries
    toEntry bs@(b:_)   = TBR.Entry (bookAuthor b) (map bookTitle bs)
    toEntry _          = error "This can't happen."

--------------------------------------------------------------------------------
-- Generic text utilities

-- | Splits the given string into tokens that contain only alphanumeric
-- characters.
tokens :: Text -> [Text]
tokens = T.split isSpace . clean
    where clean   = T.toLower . T.filter isValid
          isValid = (||) <$> isSpace <*> isAlphaNum

-- | A version of @Data.Text.IO.putStrLn@ that runs in any monad capable of
-- lifting IO operations.
putLn :: MonadIO m => Text -> m ()
putLn = liftIO . TIO.putStrLn

--------------------------------------------------------------------------------
-- Monad

-- | The BooksM monad allows access to the @BookList@.
newtype BooksM a = BM { runBM :: ScriptT (StateT BookList IO) a }
  deriving (Applicative, Functor, Monad, MonadState BookList, MonadIO)

-- | Execute the @BooksM@ monad.
runBooksM :: FilePath -> BooksM a -> IO a
runBooksM path b = do
    -- Attempt to read the contents of the file into a BookList. If the
    -- operation fails for any reason, use a default BookList.
    bookList <- runScriptT $ catchT parseList (const $ right def)
    (a, bookList') <- runStateT (runScriptT $ runBM b) bookList

    -- If the BookList has been changed, write it the file.
    when (bookList /= bookList') $ runScriptT $ putList bookList'
    return a
  where
    parseList = do 
        contents <- scriptIO (TIO.readFile path)
        document <- catchT (hoistEither $ parseDocument contents)
                           (left . T.pack)
        documentToBookList document
    putList bl = scriptIO $ do
        createDirectoryIfMissing True (takeDirectory path)
        TLIO.writeFile path (writeDocument $ bookListToDocument bl)

--------------------------------------------------------------------------------
-- Operations on Monad

-- | Query the given list of books for a book that matches the given search
-- criteria.
query' :: [Book] -> Text -> [Book]
query' books q = filter matchBook books
  where qtok      = tokens q
        matches s = qtok `isInfixOf` tokens s
        matchBook = (||) <$> matches . bookTitle
                         <*> matches . bookAuthor

-- | @query'@ that runs inside a Monad that contains the BookList. The first
-- argument is a selector that returns a list of books to query, given the
-- book list.
query :: (MonadState BookList m, Applicative m) => (BookList -> [Book]) -> Text -> m [Book]
query sel q = query' <$> getList sel <*> pure q

-- | Similar to @query@ except that only one result is expected. If the query
-- results in no books, or more than one books, an error is thrown.
queryOne :: (BookList -> [Book]) -> Text -> BooksM Book
queryOne sel q = BM $ do
    bs <- query sel q
    case bs of
        []  -> left [st|Unable to find book matching query "#{q}"|]
        [b] -> right b
        _   -> do putLn "The query matched:"
                  printBookList bs
                  left "Please refine the query."

-- | Fetch from the book list the list defined by the given selector.
getList :: (MonadState BookList m, Functor m) => (BookList -> [Book]) -> m [Book]
getList sel = sel <$> get

-- | Get the book currently being read. If the reading list is empty, or there
-- are greater than one books in it, an error is thrown.
getReading :: BooksM Book
getReading = BM $ do
    bs <- getList blReading
    case bs of
        []  -> left "You are not reading any book at the moment."
        [b] -> right b
        _   -> do putLn "You are currently reading:"
                  printBookList bs
                  left "Please provide a query."

--------------------------------------------------------------------------------
-- Formatting

formatBook :: Book -> Text
formatBook Book{..} = [st|#{bookTitle} by #{bookAuthor}|]

printBookList :: (MonadIO m) => [Book] -> m ()
printBookList bs = mapM_ putLn (formatBookList bs)

formatBookList :: [Book] -> [Text]
formatBookList bs = map format pairs
  where
    pairs         = zip [1..] bs
    numLen        = length . show
    maxNumLen     =  numLen (length bs)
    format (i, b) = [st|#{show i}. #{extraSpaces <> author} - #{title}|]
        where extraSpaces = T.replicate (maxNumLen - numLen i) " "
              author = bookAuthor b
              title  = bookTitle  b

--------------------------------------------------------------------------------
-- Commands

status :: BooksM ()
status = do
    reading <- getList blReading
    unless (null reading) $ do
        putLn "Reading:"
        printBookList reading

    tbrCount <- length <$> getList blToBeRead
    putLn [st|There are #{show tbrCount} books to be read.|]

finish :: Book -> BooksM ()
finish b = do modify step
              putLn [st|Finished reading #{formatBook b}|]
  where
    step bl@(BookList{..}) = bl {blReading = filter (/= b) blReading}

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

    when ((readCount, tbrCount) == (0, 0)) $
        putLn "No such books found."

list :: BooksM ()
list = do
    reading  <- getList blReading
    toBeRead <- getList blToBeRead

    unless (null reading) $ do
        putLn "Reading:"
        printBookList reading
        unless (null toBeRead) (putLn "")

    unless (null toBeRead) $ do
        putLn "To be read:"
        printBookList toBeRead

pick :: Text -> BooksM ()
pick q = do
    b <- queryOne blToBeRead q
    modify $ \bl@(BookList{..}) ->
        bl { blReading  = b:blReading
           , blToBeRead = filter (/= b) blToBeRead }
    putLn [st|Started reading #{formatBook b}.|]

add :: Text -> Text -> BooksM ()
add title author = BM $ do
    reading <- getList blReading
    toBeRead <- getList blToBeRead
    let readMatches = query' reading  title  `intersect`
                      query' reading author
        tbrMatches  = query' toBeRead  title `intersect`
                      query' toBeRead author
    unless (null $ readMatches <> tbrMatches) $
        left "You have already added that book."
    modify $ \bl@(BookList{..}) ->
        bl { blToBeRead = book:blToBeRead }
    putLn [st|Added #{formatBook book} to the reading list.|]
  where book = Book author title

remove :: Text -> BooksM ()
remove q = do
    b <- queryOne blToBeRead q
    modify $ \bl@(BookList{..}) ->
        bl { blToBeRead = filter (/= b) blToBeRead }
    putLn [st|Removed #{formatBook b} from the reading list.|]

stop :: Book -> BooksM ()
stop b = BM $ do
    modify $ \bl@(BookList{..}) ->
        bl { blReading = filter (/= b) blReading
           , blToBeRead = b:blToBeRead }
    putLn [st|Stopped reading #{formatBook b}.|]

--------------------------------------------------------------------------------
-- Argument parsing and dispatch

text :: Monad m => String -> m Text
text = return . T.pack

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

addParser    = Add    <$> argument text (metavar "TITLE")
                      <*> argument text (metavar "AUTHOR")
finishParser = Finish <$> optional (argument text (metavar "QUERY"))
stopParser   = Stop   <$> optional (argument text (metavar "QUERY"))
pickParser   = Pick   <$> argument text (metavar "QUERY")
removeParser = Remove <$> argument text (metavar "QUERY")
searchParser = Search <$> argument text (metavar "QUERY")

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
