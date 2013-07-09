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
import           Control.Monad.Error   (catchError)
import           Control.Monad.State
import           Data.Function         (on)
import           Data.Monoid
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           Data.Text.Lazy        (toStrict)
import           System.Random         (randomRIO)
import           TBR
import           TBR.Monad
import           Text.Shakespeare.Text (st)

--------------------------------------------------------------------------------
-- Utility operations

-- | Returns a subset of the current booklist that contains the books for which
-- the given predicate returns true.
find :: MonadState BookList m => (Book -> Bool) -> m BookList
find f = gets (Set.filter f)

-- | Returns books in the given section.
withSection :: MonadState BookList m => Section -> m BookList
withSection s = find $ (== s) . bookSection

-- | Returns the books that are marked as being read.
reading :: MonadState BookList m => m BookList
reading = withSection Reading

-- | Returns the books under the to-be-read section.
toBeRead :: MonadState BookList m => m BookList
toBeRead = withSection ToBeRead

-- | Returns all books that are not being read.
allButReading :: MonadState BookList m => m BookList
allButReading = find $ (/= Reading) . bookSection

-- | Returns all books that are under a custom list.
others :: MonadState BookList m => m BookList
others = find $ isOther . bookSection
  where isOther (Other _) = True
        isOther _         = False

-- | Returns books that are in the section that matches the given name.
matchSection :: MonadBooks m => Text -> m BookList
matchSection = findSection >=> withSection

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

-- | Finds and returns a @Section@ that matches the given query.
findSection :: MonadBooks m => Text -> m Section
findSection name
    | name `eq` "Reading"    = return Reading
    | name `eq` "To Be Read" = return ToBeRead
    | otherwise              = findOther
  where
    eq = (==) `on` tokens
    findOther = do
        matches <- gets $ Set.toAscList
                        . Set.filter match
                        . Set.map bookSection
        case matches of
            [o] -> return o
            _   -> throwError [st|Unable to find such a section.|]
      where
        match (Other x) = name `eq` x
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
list name = maybe allBooks matchSection name >>= printBookList

move :: MonadBooks m => Text -> Text -> m ()
move q lname = do
    b <- allBooks >>= query1 q
    -- Try to move into an existing section before creating a new one.
    section <- findSection lname `catchError`
               (const . return $ Other lname)
    modify $ Set.insert (b { bookSection = section })
           . Set.delete  b
    putLn [st|Moved #{formatBook b} to #{show section}|]

random :: MonadBooks m => Maybe Text -> m ()
random lname = do
    books  <- maybe toBeRead matchSection lname
    author <- select $ Set.map bookAuthor books
    book   <- select $ Set.filter ((== author) . bookAuthor) books
    putLn $ formatBook book
  where
    select s = liftIO $ do
        i <- randomRIO (0, Set.size s - 1)
        return $ Set.toAscList s !! i

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
    sec <- maybe (return ToBeRead) findSection l
    modify $ Set.insert (b { bookSection = sec })
           . Set.delete  b
    putLn [st|Stopped reading #{formatBook b}.|]

