{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving,
             QuasiQuotes, FlexibleContexts #-}
module Main (main) where

import           TBR.Data
import           TBR.Util
import           TBR.Monad

import           Data.Text              (Text)
import           Data.List              (isInfixOf, intersect)
import qualified Data.Text              as T
import           System.FilePath        ((</>))
import           System.Directory
import           Options.Applicative
import           Control.Monad.State
import           Text.Shakespeare.Text  (st)

--------------------------------------------------------------------------------
-- Operations inside the BooksM monad

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
query :: (BookList -> [Book]) -> Text -> BooksM [Book]
query sel q = query' <$> getList sel <*> pure q

-- | Similar to @query@ except that only one result is expected. If the query
-- results in no books, or more than one books, an error is thrown.
queryOne :: (BookList -> [Book]) -> Text -> BooksM Book
queryOne sel q = do
    bs <- query sel q
    case bs of
        []  -> err [st|Unable to find book matching query "#{q}"|]
        [b] -> return b
        _   -> do putLn "The query matched:"
                  printBookList bs
                  err "Please refine the query."

-- | Fetch from the book list the list defined by the given selector.
getList :: (BookList -> [Book]) -> BooksM [Book]
getList sel = sel <$> get

-- | Get the book currently being read. If the reading list is empty, or there
-- are greater than one books in it, an error is thrown.
getReading :: BooksM Book
getReading = do
    bs <- getList blReading
    case bs of
        []  -> err "You are not reading any book at the moment."
        [b] -> return b
        _   -> do putLn "You are currently reading:"
                  printBookList bs
                  err "Please provide a query."

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
add title author = do
    reading <- getList blReading
    toBeRead <- getList blToBeRead
    let readMatches = query' reading  title  `intersect`
                      query' reading author
        tbrMatches  = query' toBeRead  title `intersect`
                      query' toBeRead author
    unless (null $ readMatches <> tbrMatches) $
        err "You have already added that book."
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
stop b = do
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
