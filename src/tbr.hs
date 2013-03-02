{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving,
             QuasiQuotes, FlexibleContexts #-}
module Main (main) where

import           TBR.Types
import           TBR.Util
import           TBR.Monad

import           Data.Text              (Text)
import           Data.List              (isInfixOf, intersect)
import qualified Data.Text              as T
import           Control.Monad          (unless, forM_)
import           System.FilePath        ((</>))
import           System.Directory
import           Options.Applicative
import           Control.Monad.State    (get, modify)
import           Control.Monad.Writer   (execWriter, tell)
import           Text.Shakespeare.Text  (st)
import           Control.Monad.IO.Class (MonadIO)

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

-- | Get all books in all lists.
allBooks :: BookList -> [Book]
allBooks BookList{..} = blReading <> blToBeRead <> concatMap snd blExtra

getAllBooks :: BooksM [Book]
getAllBooks = allBooks <$> get

modifyReading, modifyToBeRead :: ([Book] -> [Book]) -> BooksM ()

modifyReading  f = modify $ \bl@BookList{..} ->
                        bl {blReading  = f blReading }
modifyToBeRead f = modify $ \bl@BookList{..} ->
                        bl {blToBeRead = f blToBeRead}

-- | Get the list of books belonging to the given extra list.
getExtraList :: Text -> BooksM (Text, [Book])
getExtraList name = do
    extra <- blExtra <$> get
    case filter (matches . fst) extra of
        [l] -> return l
        _   -> err [st|List with name "#{name}" does not exist.|]
  where  nametoks  = tokens name
         matches s = nametoks `isInfixOf` tokens s

-- | Modify the list of books belonging to the given extra list.
modifyExtraList :: Text -> ([Book] -> [Book]) -> BooksM ()
modifyExtraList name f = modifyExtraLists $ \(n, bs) ->
    if tokens name `isInfixOf` tokens n
    then f bs
    else bs

modifyExtraLists :: ((Text, [Book]) -> [Book]) -> BooksM ()
modifyExtraLists f = modify $ \bl@BookList{..} ->
    bl { blExtra = map fun blExtra }
  where fun l@(n, _) = (n, f l)

getExtraLists :: BooksM [Book]
getExtraLists = concatMap snd . blExtra <$> get

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

    tbrCount   <- length <$> getList blToBeRead
    extraCount <- length <$> getExtraLists

    putLn $ [st|There are #{show $ tbrCount + extraCount} |]
         <> [st|(#{show extraCount} extra) books to be read.|]

finish :: Maybe Text -> BooksM ()
finish mq = do
    b <- maybe getReading (queryOne blReading) mq
    modifyReading (filter (/= b))
    putLn [st|Finished reading #{formatBook b}|]

search :: Text -> Maybe Text -> BooksM ()
search q lname = do
    range <- maybe (allBs <$> get) (fmap snd . getExtraList) lname
    matches <- query (const range) q
    printBookList matches
 where allBs BookList{..} = blToBeRead <> concatMap snd blExtra

list :: Maybe Text -> BooksM ()
list = maybe listAll listOne
 where
    printSection title lst =
        unless (null lst) $ do
            putLn title
            printBookList lst

    listOne :: Text -> BooksM ()
    listOne n = getExtraList n >>= uncurry printSection 

    listAll :: BooksM ()
    listAll = do
        reading  <- getList blReading
        toBeRead <- getList blToBeRead
        extras   <- blExtra <$> get

        unless (null reading) $ do
            printSection "Reading" reading
            unless (null toBeRead) (putLn "")

        unless (null toBeRead) $ do
            printSection "To Be Read" toBeRead
            unless (null extras) (putLn "")

        unless (null extras) . runCounterT $
            forM_ extras $ \(n, l) -> do
                count <- counter
                unless (count == 1) $ putLn ""
                printSection n l

pick :: Text -> Maybe Text -> BooksM ()
pick q lname = do
    range <- maybe (allBs <$> get) (fmap snd . getExtraList) lname
    b <- queryOne (const range) q
    modifyReading (b:)
    modifyToBeRead (filter (/= b))
    modifyExtraLists (filter (/= b) . snd)
    putLn [st|Started reading #{formatBook b}.|]
 where allBs BookList{..} = blToBeRead <> concatMap snd blExtra

add :: Text -> Text -> Maybe Text -> BooksM ()
add title author lname = do
    books <- getAllBooks
    unless (null $ matches books) $
        err "You have already added that book."
    maybe modifyToBeRead modifyExtraList lname (book:)
    putLn [st|Added #{formatBook book} to the reading list.|]
  where book = Book author title
        matches bl = query' bl  title `intersect`
                     query' bl author

move :: Text -> Text -> BooksM ()
move q lname = do
    b <- queryOne allBs q
    (lst, _) <- getExtraList lname
    modifyToBeRead (filter (/= b))
    modifyExtraLists (filter (/= b) . snd)
    modifyExtraList lname (b:)
    putLn [st|Moved #{formatBook b} to #{lst}|]
 where allBs BookList{..} = blToBeRead <> concatMap snd blExtra

remove :: Text -> Maybe Text -> BooksM ()
remove q lname = do
    range <- maybe getAllBooks (fmap snd . getExtraList) lname
    b <- queryOne (const range) q
    modifyToBeRead (filter (/= b))
    modifyExtraLists (filter (/= b) . snd)
    putLn [st|Removed #{formatBook b} from the reading list.|]

stop :: Maybe Text -> Maybe Text -> BooksM ()
stop q l = do
    b <- maybe getReading (queryOne blReading) q
    modifyReading (filter (/= b))
    maybe modifyToBeRead modifyExtraList l (b:)
    putLn [st|Stopped reading #{formatBook b}.|]

--------------------------------------------------------------------------------
-- Argument parsing and dispatch

text :: Monad m => String -> m Text
text = return . T.pack

-- | Dispatch the correct subcommand based on the options.
dispatch :: Argument -> IO ()
dispatch args = runBooksM (argFile args) $
  case argCommand args of
    Add{..}    -> add addTitle addAuthor addList
    Finish{..} -> finish finishQuery
    List{..}   -> list listList
    Move{..}   -> move moveQuery moveList
    Pick{..}   -> pick pickQuery pickList
    Remove{..} -> remove removeQuery removeList
    Search{..} -> search searchQuery searchList
    Status     -> status
    Stop{..}   -> stop stopQuery stopList
    
        
-- | Represents the subcommands offered by the program.
data Command = Add    { addTitle    :: Text
                      , addAuthor   :: Text
                      , addList     :: Maybe Text }
             | Finish { finishQuery :: Maybe Text }
             | List   { listList    :: Maybe Text }
             | Move   { moveQuery   :: Text
                      , moveList    :: Text }
             | Pick   { pickQuery   :: Text
                      , pickList    :: Maybe Text }
             | Remove { removeQuery :: Text
                      , removeList  :: Maybe Text}
             | Search { searchQuery :: Text
                      , searchList  :: Maybe Text }
             | Status 
             | Stop   { stopQuery   :: Maybe Text
                      , stopList    :: Maybe Text }

-- | Represents all the command line arguments accepted by the program.
data Argument = Argument { argFile    :: FilePath
                         , argCommand :: Command
                         }

addParser, finishParser, listParser, moveParser, pickParser, removeParser,
    searchParser, stopParser :: Parser Command

listOption :: Parser (Maybe Text)
listOption = optional $
    nullOption $ reader (return . T.pack) <>
               ( long "list"
              <> short 'l'
              <> metavar "LIST"
              <> help "The target list." )

queryParser :: Parser Text
queryParser = argument text (metavar "QUERY")

addParser    = Add    <$> argument text (metavar "TITLE")
                      <*> argument text (metavar "AUTHOR")
                      <*>          listOption
finishParser = Finish <$> optional queryParser
listParser   = List   <$>          listOption
moveParser   = Move   <$>          queryParser
                      <*> argument text (metavar "LIST")
pickParser   = Pick   <$>          queryParser
                      <*>          listOption
removeParser = Remove <$>          queryParser
                      <*>          listOption
searchParser = Search <$>          queryParser
                      <*>          listOption
stopParser   = Stop   <$> optional queryParser
                      <*>          listOption

-- | Parser for all subcommands.
commandParser :: Parser Command
commandParser = subparser . execWriter $ do
    cmd addParser     "add"    "Add a book."
    cmd finishParser  "finish" "Mark a book finished."
    cmd listParser    "list"   "List all books to be read."
    cmd moveParser    "move"   "Move a book between lists."
    cmd pickParser    "pick"   "Start reading a book."
    cmd removeParser  "remove" "Remove a book."
    cmd searchParser  "search" "Search for a book in the list."
    cmd (pure Status) "status" "Show reading status."
    cmd stopParser    "stop"   "Stop reading a book."
  where
    cmd parser name desc = tell $ command name (info parser $ progDesc desc)

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
    dispatch =<< execParser
                    (info (helper <*> parser)
                          ( fullDesc
                         <> header "A tool to maintain reading lists."))
