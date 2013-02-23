{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TBR.Monad
    ( BooksM
    , runBooksM
    , err
    ) where

import           TBR.Data
import           TBR.Reader
import           TBR.Writer
import           TBR.Script

import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Default
import           Control.Error          (catchT, left, right, hoistEither)
import           System.FilePath        (takeDirectory)
import           System.Directory
import qualified Data.Text.Lazy.IO      as TLIO
import           Control.Applicative
import           Control.Monad.State

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

err :: T.Text -> BooksM a
err = BM . left
