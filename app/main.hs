{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Main (main) where

import           Control.Monad.Writer (execWriter, tell)
import qualified Data.Configurator    as Config
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Options.Applicative
import           System.Directory
import           System.FilePath      ((</>))
import           TBR.Core
import           TBR.Monad
import           TBR.Script

--------------------------------------------------------------------------------
-- Argument parsing and dispatch

text :: Monad m => String -> m Text
text = return . Text.pack

readConfiguration :: Argument -> IO (Command, Configuration)
readConfiguration Argument{..} = runScriptT . scriptIO $ do
    home <- getHomeDirectory
    let defaultTarget = home </> ".tbr" </> "tbr.txt"

    config <- Config.load [Config.Optional "$(HOME)/.tbr/config"]
    (argCommand,)    <$>
      (Configuration <$> Config.lookupDefault
                            defaultTarget config "tbr.target"
                     <*> pure argDryRun)

-- | Dispatch the correct subcommand based on the options.
dispatch :: Command -> Configuration -> IO ()
dispatch cmd = flip runBooksM $
  case cmd of
    Add{..}    -> add addTitle addAuthor addList
    Finish{..} -> finish finishQuery
    List{..}   -> list listList
    Move{..}   -> move moveQuery moveList
    Random{..} -> random randomList
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
             | Random { randomList  :: Maybe Text }
             | Remove { removeQuery :: Text       }
             | Search { searchQuery :: Text       }
             | Start  { startQuery  :: Text       }
             | Status
             | Stop   { stopQuery   :: Maybe Text
                      , stopList    :: Maybe Text }
    deriving (Show, Eq)

-- | Represents all the command line arguments accepted by the program.
data Argument = Argument { argDryRun  :: Bool
                         , argCommand :: Command
                         } deriving (Show, Eq)

addParser, finishParser, listParser, moveParser, randomParser, removeParser,
    searchParser, startParser, stopParser :: Parser Command

listArgument :: Parser Text
listArgument = argument text (metavar "LIST")

queryParser :: Parser Text
queryParser = argument text (metavar "QUERY")

addParser    = Add    <$> argument text (metavar "TITLE")
                      <*> argument text (metavar "AUTHOR")
                      <*> optional listArgument
finishParser = Finish <$> optional queryParser
listParser   = List   <$> optional listArgument
moveParser   = Move   <$>          queryParser
                      <*>          listArgument
randomParser = Random <$> optional listArgument
removeParser = Remove <$>          queryParser
searchParser = Search <$>          queryParser
startParser  = Start  <$>          queryParser
stopParser   = Stop   <$> optional queryParser
                      <*> optional listArgument

-- | Parser for all subcommands.
commandParser :: Parser Command
commandParser = subparser . execWriter $ do
    cmd addParser     "add"    "Add a book."
    cmd finishParser  "finish" "Mark a book finished."
    cmd listParser    "list"   "List all books to be read."
    cmd moveParser    "move"   "Move a book between lists."
    cmd randomParser  "random" "Suggest a random book."
    cmd removeParser  "remove" "Remove a book."
    cmd searchParser  "search" "Search for a book in the list."
    cmd startParser   "start"  "Start reading a book."
    cmd (pure Status) "status" "Show reading status."
    cmd stopParser    "stop"   "Stop reading a book."
  where
    cmd parser name desc = tell $ command name (info parser $ progDesc desc)

-- | Build the complete command line argument parser.
argumentParser :: Parser Argument
argumentParser = Argument
    <$> switch (long "dry-run" <> short 'n' <> help "Don't change the file.")
    <*> commandParser

main :: IO ()
main = execParser (info (helper  <*> argumentParser)
                        (fullDesc <> header "A tool to maintain reading lists."))
   >>= readConfiguration
   >>= uncurry dispatch
