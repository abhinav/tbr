{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards,
             ScopedTypeVariables #-}
module TBR.Data
    ( Book(..)
    , BookList(..)
    , documentToBookList
    , bookListToDocument
    ) where

import           TBR.Util
import qualified TBR.Types              as TBR
import           TBR.Script
import           Data.Text              (Text)
import           Data.List              (sortBy, groupBy)
import           Data.Default
import           Control.Error          (left, right)
import           Control.Applicative
import           Text.Shakespeare.Text  (st)

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
documentToBookList
    :: forall m. (Monad m, Functor m)
    => TBR.Document
    -> ScriptT m BookList
documentToBookList TBR.Document{..} = BookList
                                  <$> blockBooks "Reading"
                                  <*> blockBooks "To be read"
                                  <*> blockBooks "Low priority"
  where
    blockBooks :: Text -> ScriptT m [Book]
    blockBooks name = concatMap entryToBooks <$> lookupBlock name

    -- Get the entries for the block with the given name or fail.
    lookupBlock :: Text -> ScriptT m [TBR.Entry]
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


