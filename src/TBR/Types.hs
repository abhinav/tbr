module TBR.Types
    ( Author
    , Book(..)
    , BookList
    , Section(..)
    , Title
    ) where

import           Data.Monoid
import           Data.Ord    (comparing)
import           Data.Set    (Set)
import           Data.Text   (Text)

type Author = Text
type Title  = Text
type BookList = Set Book

data Section = Reading
             | ToBeRead
             | Other Text
    deriving (Show, Eq, Ord)

data Book = Book { bookTitle   :: Title
                 , bookAuthor  :: Author
                 , bookSection :: Section
                 } deriving (Show, Eq)

instance Ord Book where
    compare = comparing bookSection <>
              comparing bookAuthor  <>
              comparing bookTitle

