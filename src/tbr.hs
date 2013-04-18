{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
module Main (main) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Writer   (execWriter, tell)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Lazy         (toStrict)
import           Options.Applicative
import           System.Directory
import           System.FilePath        ((</>))
import           TBR.Monad
import           TBR.Types
import           TBR.Util
import           TBR.Writer
import           Text.Shakespeare.Text  (st)

--------------------------------------------------------------------------------
-- Operations inside the BooksM monad

-- | Returns a subset of the current booklist that contains the books for which
-- the given predicate returns true.
find :: (Book -> Bool) -> BooksM BookList
find f = gets (Set.filter f)

-- | Returns the books that are marked as being read.
reading :: BooksM BookList
reading = find $ (== Reading) . bookSection

-- | Returns the books under the to-be-read section.
toBeRead :: BooksM BookList
toBeRead = find $ (== ToBeRead) . bookSection

-- | Returns all books that are not being read.
allButReading :: BooksM BookList
allButReading = find $ (/= Reading) . bookSection

-- | Returns all books that are under a custom list.
others :: BooksM BookList
others = find $ isOther . bookSection
  where isOther (Other _) = True
        isOther _         = False

-- | Returns all books that are under the given list.
other :: Text -> BooksM BookList
other t = find $ (== Other t) . bookSection

-- | Queries the given list of books for books that match the given criteria.
query :: Text -> BookList -> BookList
query q = Set.filter matcher
  where matcher = (||) <$> match . bookTitle
                       <*> match . bookAuthor
        match   = tokenMatch q

-- | Expects exactly one book and throws an error if that is not satisfied.
expect1 :: BookList -> BooksM Book
expect1 bl = case Set.toAscList bl of
                []  -> err [st|Unable to find such a book.|]
                [b] -> return b
                _   -> do putLn "The query matched:"
                          printBookList bl
                          err "Please refine the query."

-- | Finds exactly one book that matches the given query and throws an error
-- otherwise.
query1 :: Text -> BookList -> BooksM Book
query1 q = expect1 . query q

-- | Returns all books in the list.
allBooks :: BooksM BookList
allBooks = get

--------------------------------------------------------------------------------
-- Formatting

-- | Formats a single book in a nice readable format.
formatBook :: Book -> Text
formatBook Book{..} = [st|#{bookTitle} by #{bookAuthor}|]

-- | Prints the given list of books.
printBookList :: (MonadIO m) => BookList -> m ()
printBookList = puts . toStrict . writeBookList

--------------------------------------------------------------------------------
-- Commands

add :: Text -> Text -> Maybe Text -> BooksM ()
add title author lname = do
    books <- allBooks
    unless (Set.null $ matches books) $
        err "You have already added that book."
    modify $ Set.insert book
    putLn [st|Added #{formatBook book} to the reading list.|]
  where book = Book title author (maybe ToBeRead Other lname)
        matches bl = query title  bl `Set.intersection`
                     query author bl

finish :: Maybe Text -> BooksM ()
finish q = do
    b <- reading >>= maybe expect1 query1 q
    modify $ Set.delete b
    putLn [st|Finished reading #{formatBook b}|]

list :: Maybe Text -> BooksM ()
list name = maybe allBooks other name >>= printBookList

move :: Text -> Text -> BooksM ()
move q lname = do
    b <- allBooks >>= query1 q
    modify $ Set.insert (b { bookSection = Other lname })
           . Set.delete  b
    putLn [st|Moved #{formatBook b} to #{lname}|]

remove :: Text -> BooksM ()
remove q = do
    b <- allButReading >>= query1 q
    modify $ Set.delete b
    putLn [st|Removed #{formatBook b} from the reading list.|]

search :: Text -> BooksM ()
search q = query q <$> allBooks >>= printBookList

start :: Text -> BooksM ()
start q = do
    b <- allButReading >>= query1 q
    modify $ Set.insert (b { bookSection = Reading })
           . Set.delete  b
    putLn [st|Started reading #{formatBook b}.|]

status :: BooksM ()
status = do
    rl <- reading
    unless (Set.null rl) $
        printBookList rl

    tbrCount   <- Set.size <$> toBeRead
    otherCount <- Set.size <$> others

    putLn $ [st|There are #{show $ tbrCount + otherCount} |]
         <> [st|(#{show otherCount} other) books to be read.|]

stop :: Maybe Text -> Maybe Text -> BooksM ()
stop q l = do
    b <- reading >>= maybe expect1 query1 q
    modify $ Set.insert (b { bookSection = maybe ToBeRead Other l })
           . Set.delete  b
    putLn [st|Stopped reading #{formatBook b}.|]

--------------------------------------------------------------------------------
-- Argument parsing and dispatch

text :: Monad m => String -> m Text
text = return . Text.pack

-- | Dispatch the correct subcommand based on the options.
dispatch :: Argument -> IO ()
dispatch args = runBooksM (argFile args) $
  case argCommand args of
    Add{..}    -> add addTitle addAuthor addList
    Finish{..} -> finish finishQuery
    List{..}   -> list listList
    Move{..}   -> move moveQuery moveList
    Remove{..} -> remove removeQuery
    Search{..} -> search searchQuery
    Start{..}  -> start startQuery
    Status     -> status
    Stop{..}   -> stop stopQuery stopList


-- | Represents the subcommands offered by the program.
data Command = Add    { addTitle    :: Text
                      , addAuthor   :: Text
                      , addList     :: Maybe Text }
             | Finish { finishQuery :: Maybe Text }
             | List   { listList    :: Maybe Text }
             | Move   { moveQuery   :: Text
                      , moveList    :: Text       }
             | Remove { removeQuery :: Text       }
             | Search { searchQuery :: Text       }
             | Start  { startQuery  :: Text       }
             | Status
             | Stop   { stopQuery   :: Maybe Text
                      , stopList    :: Maybe Text }

-- | Represents all the command line arguments accepted by the program.
data Argument = Argument { argFile    :: FilePath
                         , argCommand :: Command
                         }

addParser, finishParser, listParser, moveParser, removeParser, searchParser,
    startParser, stopParser :: Parser Command

listOption :: Parser (Maybe Text)
listOption = optional $
    nullOption $ reader (return . Text.pack) <>
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
removeParser = Remove <$>          queryParser
searchParser = Search <$>          queryParser
startParser  = Start  <$>          queryParser
stopParser   = Stop   <$> optional queryParser
                      <*>          listOption

-- | Parser for all subcommands.
commandParser :: Parser Command
commandParser = subparser . execWriter $ do
    cmd addParser     "add"    "Add a book."
    cmd finishParser  "finish" "Mark a book finished."
    cmd listParser    "list"   "List all books to be read."
    cmd moveParser    "move"   "Move a book between lists."
    cmd removeParser  "remove" "Remove a book."
    cmd searchParser  "search" "Search for a book in the list."
    cmd startParser   "start"  "Start reading a book."
    cmd (pure Status) "status" "Show reading status."
    cmd stopParser    "stop"   "Stop reading a book."
  where
    cmd parser name desc = tell $ command name (info parser $ progDesc desc)

-- | Build the complete command line argument parser.
getArgumentParser :: FilePath -> Parser Argument
getArgumentParser appDir =
    Argument
        <$> strOption (long "file"              <>
                       short 'f'                <>
                       metavar "FILE"           <>
                       help "The reading list." <>
                       value filepath            )
        <*> commandParser
  where
    filepath = appDir </> "tbr.txt"

main :: IO ()
main = do
    parser <- getArgumentParser <$> getAppUserDataDirectory "tbr"
    dispatch =<<
        execParser (info (helper  <*> parser)
                         (fullDesc <> header "A tool to maintain reading lists."))
