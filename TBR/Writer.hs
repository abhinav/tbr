{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module TBR.Writer (writeBookList) where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Set               as Set
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder
import           TBR.Types
import           TBR.Util

-- | A writer used to incrementally build lazy text.
type TextBuilder = Writer Builder

runTextBuilder :: TextBuilder a -> TL.Text
runTextBuilder = toLazyText . execWriter

tellText :: T.Text -> TextBuilder ()
tellText = void . tell . fromText

newln :: TextBuilder ()
newln = void $ tell "\n"

writeHeader :: T.Text -> TextBuilder ()
writeHeader hdr = void $ tellText hdr      >> newln
               >> underline (T.length hdr) >> newln
  where underline n = tellText $ T.replicate (fromIntegral n) "="

writeBookList :: BookList -> TL.Text
writeBookList bl = runTextBuilder $ forM_ sections $ \sec -> do
    let secBooks = Set.filter ((== sec) . bookSection) bl
        authors  = Set.toAscList (Set.map bookAuthor secBooks)
        numAuth  = length authors

    -- Start a new section
    writeHeader (sectionHeader sec) >> newln

    -- For each author in the section, write all the books by that author,
    -- incrementing the counter once per author.
    runCounterT $ forM_ authors $ \auth -> do
        pos <- counter
        let books = authorBooks auth secBooks
        unless (null books) $ lift $ do
            tell (fromString $ show pos) >> tell ". "
            tellText $ spaces (numLen numAuth - numLen pos)
            case books of
                -- Use single line format for one book
                [Book{..}] -> tellText bookTitle
                           >> tell " - "
                           >> tellText bookAuthor
                           >> newln
                -- Multiple books by the author.
                _          -> do
                    tellText auth >> newln
                    forM_ books $ \Book{..} -> do
                        tellText $ spaces (numLen numAuth + 2)
                        tell "-  "
                        tellText bookTitle
                        newln
    newln
  where
    sections = Set.toAscList (Set.map bookSection bl)

    sectionHeader Reading   = "Reading"
    sectionHeader ToBeRead  = "To Be Read"
    sectionHeader (Other x) = x

    authorBooks auth = Set.toAscList . Set.filter ((== auth) . bookAuthor)

    numLen = length . show
    spaces = flip T.replicate " "
