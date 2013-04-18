{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TBR.Monad
    ( BooksM
    , runBooksM
    , err
    ) where

import           Control.Applicative
import           Control.Error       (catchT, hoistEither, left, right)
import           Control.Monad.State
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Text.Lazy.IO   as TLIO
import           System.Directory
import           System.FilePath     (takeDirectory)
import           TBR.Reader
import           TBR.Script
import           TBR.Types
import           TBR.Writer

-- | The BooksM monad allows access to the @BookList@.
newtype BooksM a = BM { runBM :: ScriptT (StateT BookList IO) a }
  deriving (Applicative, Functor, Monad, MonadState BookList, MonadIO)

-- | Execute the @BooksM@ monad.
runBooksM :: FilePath -> Bool -> BooksM a -> IO a
runBooksM path dryrun b = do
    -- Attempt to read the contents of the file into a BookList. If the
    -- operation fails for any reason, use a default BookList.
    bookList <- runScriptT $ catchT parseList (const $ right Set.empty)
    (a, bookList') <- runStateT (runScriptT $ runBM b) bookList

    -- If the BookList has been changed, write it the file.
    when (bookList /= bookList' && not dryrun) $
        runScriptT $ putList bookList'

    return a
  where
    parseList = do
        contents <- scriptIO (TIO.readFile path)
        catchT (hoistEither $ readBookList contents)
               (left . T.pack)
    putList bl = scriptIO $ do
        createDirectoryIfMissing True (takeDirectory path)
        TLIO.writeFile path (writeBookList bl)

err :: T.Text -> BooksM a
err = BM . left
