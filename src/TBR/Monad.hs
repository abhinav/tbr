{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
module TBR.Monad
    ( BooksM
    , runBooksM
    , throwError
    , MonadBooks
    , Configuration(..)
    ) where

import           Control.Applicative
import           Control.Error          (catchT, hoistEither, left, right)
import           Control.Monad
import           Control.Monad.Error    (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State    (MonadState, StateT, runStateT)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy.IO      as TLIO
import           System.Directory
import           System.FilePath        (takeDirectory)
import           TBR.Reader
import           TBR.Script
import           TBR.Types
import           TBR.Writer

class (MonadError T.Text m, MonadIO m, MonadState BookList m) => MonadBooks m

-- | The BooksM monad allows access to the @BookList@.
newtype BooksM a = BM { runBM :: ScriptT (StateT BookList IO) a }
  deriving ( Applicative
           , Functor
           , Monad
           , MonadError T.Text
           , MonadIO
           , MonadState BookList
           )

instance MonadBooks BooksM

-- | Execute the @BooksM@ monad.
runBooksM :: Configuration -> BooksM a -> IO a
runBooksM Configuration{..} b = do
    -- Attempt to read the contents of the file into a BookList. If the
    -- operation fails for any reason, use a default BookList.
    bookList <- runScriptT $ catchT parseList (const $ right Set.empty)
    (a, bookList') <- runStateT (runScriptT $ runBM b) bookList

    -- If the BookList has been changed, write it the file.
    when (bookList /= bookList' && not configDryRun) $
        runScriptT $ putList bookList'

    return a
  where
    parseList = do
        contents <- scriptIO (TIO.readFile configTarget)
        catchT (hoistEither $ readBookList contents)
               (left . T.pack)
    putList bl = scriptIO $ do
        createDirectoryIfMissing True (takeDirectory configTarget)
        TLIO.writeFile configTarget (writeBookList bl)

data Configuration = Configuration { configTarget :: FilePath
                                   -- ^ File containing the reading list.
                                   , configDryRun :: Bool
                                   -- ^ Whether changes to the reading list
                                   -- should be committed.
                                   } deriving (Show, Eq)
