{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards,
             ScopedTypeVariables #-}
module TBR.Types
    ( Entry(..)
    , Block(..)
    , Document
    , Book(..)
    , BookList(..)
    , documentToBookList
    , bookListToDocument
    ) where

import           TBR.Util
import           TBR.Script

import           Data.Text              (Text)
import           Data.List              (sortBy, groupBy)
import           Data.Default           (Default(..))
import           Control.Error          (left, right)
import           Control.Applicative    ((<$>), (<*>), pure)
import           Text.Shakespeare.Text  (st)

--------------------------------------------------------------------------------
-- Low level document representation

-- | Represents an author and one or more books from that author.
data Entry = Entry { entryAuthor :: Text
                   , entryBooks  :: [Text] }
        deriving (Show, Eq)

-- | Represents a group of entries with a title.
data Block = Block { blockHeader  :: Text
                   , blockEntries :: [Entry] }
        deriving (Show, Eq)

-- | Represents multiple blocks.
type Document = [Block]

--------------------------------------------------------------------------------
-- High level book list representation

data Book = Book { bookAuthor :: Text
                 , bookTitle  :: Text }
    deriving (Show, Eq)

data BookList = BookList { blReading  :: [Book]
                         , blToBeRead :: [Book]
                         , blExtra    :: [(Text, [Book])] }
    deriving (Show, Eq)

instance Default BookList where
    def = BookList [] [] []

--------------------------------------------------------------------------------
-- Data type conversion

-- | Convert a @Document@ into a @BookList@. Throws an error if the three
-- sections are absent.
documentToBookList
    :: forall m. (Monad m, Functor m)
    => Document
    -> ScriptT m BookList
documentToBookList documentBlocks = BookList
                                <$> blockBooks "Reading"
                                <*> blockBooks "To be read"
                                <*> pure extraBlocks
  where
    -- Get the list of books from the given block
    blockBooks :: Text -> ScriptT m [Book]
    blockBooks name = concatMap entryToBooks <$> lookupBlock name

    -- The extra blocks
    extraBlocks :: [(Text, [Book])]
    extraBlocks = map blockToPair $ filter (not . skip . blockHeader)
                                           documentBlocks
      where match a b = tokens a == tokens b
            skip = (||) <$> match "Reading" <*> match "To be read"
            blockToPair Block{..} = ( blockHeader
                                    , concatMap entryToBooks blockEntries)

    -- Get the entries for the block with the given name or fail.
    lookupBlock :: Text -> ScriptT m [Entry]
    lookupBlock name = 
        case filter (match . blockHeader) documentBlocks of
            []  -> left [st|Could not find the section: #{name}|]
            [x] -> right (blockEntries x)
            _   -> left [st|Multiple "#{name}" sections found.|]
        where nameTok = tokens name
              match   = (== nameTok) . tokens

    -- Convert an entry to a list of books
    entryToBooks :: Entry -> [Book]
    entryToBooks Entry{..} = map (Book entryAuthor) entryBooks

-- | Convert a @BookList@ into a @Document@.
bookListToDocument :: BookList -> Document
bookListToDocument BookList{..} = 
    [ toBlock "Reading"      blReading
    , toBlock "To Be Read"   blToBeRead] ++ map (uncurry toBlock) blExtra
  where
    bookCompare a b    = compare (bookAuthor a) (bookAuthor b)
    bookGroup   a b    = bookAuthor a == bookAuthor b
    toBlock name books = let sorted = sortBy bookCompare books
                             grouped = groupBy bookGroup sorted
                             entries = map toEntry grouped
                         in Block name entries

    toEntry bs@(b:_)   = Entry (bookAuthor b) (map bookTitle bs)
    toEntry _          = error "This can't happen."



