{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Control.Monad.Writer (execWriter, tell)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Options.Applicative
import           System.Directory
import           System.FilePath      ((</>))
import           TBR.Core
import           TBR.Monad

--------------------------------------------------------------------------------
-- Argument parsing and dispatch

text :: Monad m => String -> m Text
text = return . Text.pack

-- | Dispatch the correct subcommand based on the options.
dispatch :: Argument -> IO ()
dispatch Argument{..} = runBooksM argFile argDryRun $
  case argCommand of
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
                         , argDryRun  :: Bool
                         , argCommand :: Command
                         }

addParser, finishParser, listParser, moveParser, removeParser, searchParser,
    startParser, stopParser :: Parser Command

listOption :: Parser (Maybe Text)
listOption = optional . nullOption  $
       reader ( return . Text.pack) <>
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
        <*> switch (long "dry-run" <> short 'n' <>
                    help "Don't change the file.")
        <*> commandParser
  where
    filepath = appDir </> "tbr.txt"

main :: IO ()
main = do
    parser <- getArgumentParser <$> getAppUserDataDirectory "tbr"
    dispatch =<<
        execParser (info (helper  <*> parser)
                         (fullDesc <> header "A tool to maintain reading lists."))
