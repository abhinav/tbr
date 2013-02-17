{-# LANGUAGE OverloadedStrings #-}
module Parse
    ( document
    , Author
    , Header
    , Book
    , Entry(..)
    , Block(..)
    ) where

import Data.Text (Text, pack)
import Control.Monad (void)
import Control.Applicative ((<|>), (<$>), (<*), (<*>))
import Data.Attoparsec.Text
    ( Parser, skipMany, endOfLine, takeTill, isEndOfLine, endOfLine, manyTill
    , anyChar, string, skipSpace, many1, char, decimal, satisfy, space, count
    , sepBy, many', option )

-- TODO move types to Types module

type Author = Text
type Header = Text
type Book   = Text

data Entry = Entry { entryAuthor :: Author
                   , entryBooks  :: [Book] }
        deriving (Show, Eq)

data Block = Block { blockHeader  :: Header
                   , blockEntries :: [Entry] }
        deriving (Show, Eq)

skipLines :: Parser ()
skipLines = skipMany endOfLine

-- | Consumes the returns the rest of the line. The newline character is
-- consumed but not returned.
restOfLine :: Parser Text
restOfLine = takeTill isEndOfLine <* endOfLine

-- | Matches a comment and returns the comment contents.
comment :: Parser Text
comment = open >> (pack <$> manyTill anyChar close)
    where open  = string "<!--" >> skipSpace
          close = skipSpace >> string "-->"

header :: Parser Header
header = restOfLine <* many1 (char '=')

entry :: Parser Entry
entry = itemNumber >> (singleBook <|> multipleBooks)
    where itemNumber = (decimal :: Parser Integer) <* char '.' <* skipSpace

singleBook :: Parser Entry
singleBook = flip Entry <$> ((:[]) . pack <$> book)
                        <*> author
    where book   = manyTill (satisfy $ not . isEndOfLine) (string " - ")
          author = restOfLine

multipleBooks :: Parser Entry
multipleBooks = Entry <$> author
                      <*> books
    where author = restOfLine
          books  = do indentSize <- length <$> many1 space
                      sepBy bookItem (count indentSize space)
          bookItem = char '-' >> skipSpace >> restOfLine

block :: Parser Block
block = Block <$> (header <* skipLines)
              <*> many' entry

document :: Parser [Block]
document = option () (void comment)
        >> skipLines
        >> many1 block


