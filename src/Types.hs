module Types
    ( Author
    , Header
    , Book
    , Entry(..)
    , Block(..)
    , Document(..)
    ) where

import Data.Text (Text)

type Author = Text
type Header = Text
type Book   = Text

data Entry = Entry { entryAuthor :: Author
                   , entryBooks  :: [Book] }
        deriving (Show, Eq)

data Block = Block { blockHeader  :: Header
                   , blockEntries :: [Entry] }
        deriving (Show, Eq)

data Document = Document { documentBlocks :: [Block] }
