{-# LANGUAGE OverloadedStrings #-}
module TBR.Reader (parseDocument) where

import TBR.Types
import Data.Text (Text, pack)
import Control.Monad (void)
import Control.Applicative ((<|>), (<$>), (<*), (<*>))
import Data.Attoparsec.Text

-- | Skip a bunch of newlines.
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

document :: Parser Document
document = fmap Document $
    option () (void comment) >> skipLines >> many1 block

parseDocument :: Text -> Either String Document
parseDocument = parseOnly document
