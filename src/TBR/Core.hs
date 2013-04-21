{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
module TBR.Core
    ( add
    , finish
    , list
    , move
    , random
    , remove
    , search
    , start
    , status
    , stop
    ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.Function         (on)
import           Data.Monoid
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           Data.Text.Lazy        (toStrict)
import           System.Random         (randomRIO)
import           TBR.Monad
import           TBR.Types
import           TBR.Util
import           TBR.Writer
import           Text.Shakespeare.Text (st)

--------------------------------------------------------------------------------
-- Utility operations

-- | Returns a subset of the current booklist that contains the books for which
-- the given predicate returns true.
find :: MonadState BookList m => (Book -> Bool) -> m BookList
find f = gets (Set.filter f)

-- | Returns the books that are marked as being read.
reading :: MonadState BookList m => m BookList
reading = find $ (== Reading) . bookSection

-- | Returns the books under the to-be-read section.
toBeRead :: MonadState BookList m => m BookList
toBeRead = find $ (== ToBeRead) . bookSection

-- | Returns all books that are not being read.
allButReading :: MonadState BookList m => m BookList
allButReading = find $ (/= Reading) . bookSection

-- | Returns all books that are under a custom list.
others :: MonadState BookList m => m BookList
others = find $ isOther . bookSection
  where isOther (Other _) = True
        isOther _         = False

-- | Returns all books that are under the given list.
other :: MonadState BookList m => Text -> m BookList
other t = find $ (== Other t) . bookSection

-- | Same as @other@ except the section name is matched against existing
-- section names.
findOther :: MonadBooks m => Text -> m BookList
findOther = findSection >=> other

-- | Queries the given list of books for books that match the given criteria.
query :: Text -> BookList -> BookList
query q = Set.filter matcher
  where matcher = (||) <$> match . bookTitle
                       <*> match . bookAuthor
        match   = tokenMatch q

-- | Expects exactly one book and throws an error if that is not satisfied.
expect1 :: MonadBooks m => BookList -> m Book
expect1 bl = case Set.toAscList bl of
                []  -> throwError [st|Unable to find such a book.|]
                [b] -> return b
                _   -> do putLn "The query matched:"
                          printBookList bl
                          throwError "Please refine the query."

-- | Finds exactly one book that matches the given query and throws an error
-- otherwise.
query1 :: MonadBooks m => Text -> BookList -> m Book
query1 q = expect1 . query q

-- | Returns all books in the list.
allBooks :: MonadState BookList m => m BookList
allBooks = get

-- | Finds the section name that matches the given query.
findSection :: MonadBooks m => Text -> m Text
findSection l = do
    matches <- gets ( Set.toAscList
                    . Set.filter match
                    . Set.map bookSection )
    case matches of
        [Other n] -> return n
        _         -> throwError [st|Unable to find such a section.|]
  where
    match (Other x) = l `tokenMatch` x
    match  _        = False

--------------------------------------------------------------------------------
-- Formatting

-- | Formats a single book in a nice readable format.
formatBook :: Book -> Text
formatBook Book{..} = [st|#{bookTitle} by #{bookAuthor}|]

-- | Prints the given list of books.
printBookList :: (MonadIO m) => BookList -> m ()
printBookList = puts . toStrict . writeBookList

--------------------------------------------------------------------------------
-- Commands

add :: MonadBooks m => Text -> Text -> Maybe Text -> m ()
add title author lname = do
    books <- allBooks
    unless (Set.null $ matches books) $
        throwError "You have already added that book."
    modify $ Set.insert book
    putLn [st|Added #{formatBook book} to the reading list.|]
  where book       = Book title author' (maybe ToBeRead Other lname)
        matches bl = query title bl `Set.intersection` query author bl
        author'    = capitalize author

finish :: MonadBooks m => Maybe Text -> m ()
finish q = do
    b <- reading >>= maybe expect1 query1 q
    modify $ Set.delete b
    putLn [st|Finished reading #{formatBook b}|]

list :: Maybe Text -> BooksM ()
list name = maybe allBooks findOther name >>= printBookList

move :: MonadBooks m => Text -> Text -> m ()
move q lname = do
    b <- allBooks >>= query1 q
    modify $ Set.insert (b { bookSection = section })
           . Set.delete  b
    putLn [st|Moved #{formatBook b} to #{show section}|]
  where
    section | lname `eq` "Reading"    = Reading
            | lname `eq` "To Be Read" = ToBeRead
            | otherwise               = Other lname
    eq = (==) `on` tokens

random :: MonadBooks m => Maybe Text -> m ()
random lname = do
    bs <- maybe toBeRead findOther lname
    i  <- liftIO $ randomRIO (0, Set.size bs - 1)
    let b = Set.toAscList bs !! i
    putLn $ formatBook b

remove :: MonadBooks m => Text -> m ()
remove q = do
    b <- allButReading >>= query1 q
    modify $ Set.delete b
    putLn [st|Removed #{formatBook b} from the reading list.|]

search :: Text -> BooksM ()
search q = query q <$> allBooks >>= printBookList

start :: MonadBooks m => Text -> m ()
start q = do
    b <- allButReading >>= query1 q
    modify $ Set.insert (b { bookSection = Reading })
           . Set.delete  b
    putLn [st|Started reading #{formatBook b}.|]

status :: (Functor m, MonadBooks m) => m ()
status = do
    rl <- reading
    unless (Set.null rl) $
        printBookList rl

    tbrCount   <- Set.size <$> toBeRead
    otherCount <- Set.size <$> others

    putLn $ [st|There are #{show $ tbrCount + otherCount} |]
         <> [st|(#{show otherCount} other) books to be read.|]

stop :: (Functor m, MonadBooks m) => Maybe Text -> Maybe Text -> m ()
stop q l = do
    b <- reading >>= maybe expect1 query1 q
    sec <- maybe (return ToBeRead) (fmap Other . findSection) l
    modify $ Set.insert (b { bookSection = sec })
           . Set.delete  b
    putLn [st|Stopped reading #{formatBook b}.|]

