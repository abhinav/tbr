{-# LANGUAGE OverloadedStrings #-}
module TBR.Reader (readBookList) where

import           Control.Applicative
import           Control.Monad        (void)
import           Data.Attoparsec.Text
import           Data.Monoid
import qualified Data.Set             as Set
import           Data.Text            (Text, pack)
import           TBR.Types
import           TBR.Util

-- | Skips consecutive newlines.
skipLines :: Parser ()
skipLines = skipMany endOfLine

-- | Consumes the returns the rest of the line. The newline character is
-- consumed but is not a part of the text.
restOfLine :: Parser Text
restOfLine = takeTill isEndOfLine <* endOfLine

-- | Matches a comment and returns the comment contents.
comment :: Parser Text
comment = open >> (pack <$> manyTill anyChar close)
    where open  = string "<!--" >> skipSpace
          close = skipSpace >> string "-->"

-- | Reads section headers.
section :: Parser Section
section = readSection <$>
          restOfLine  <*  many1 (char '=')
                      <*  skipLines
  where
    readSection s
       | tokens s == tokens "Reading"    = Reading
       | tokens s == tokens "To Be Read" = ToBeRead
       | otherwise                       = Other s

-- | Reads a single author's entry that can contain one or more books.
entry :: Section -> Parser BookList
entry sec = do
    void $ (decimal :: Parser Integer) <* char '.' <* skipSpace
    (Set.singleton <$> singleBook sec <|> multipleBooks sec) <* skipLines

-- | Reads a single book in the format @TITLE - AUTHOR@.
singleBook :: Section -> Parser Book
singleBook sec = Book <$> readTitle
                      <*> restOfLine -- author
                      <*> pure sec
  where
    readTitle = pack  <$> manyTill (satisfy $ not . isEndOfLine)
                                   (string " - ")

-- | Reads multiple books by the same author in the format:
--
--      AUTHOR
--      - TITLE
--      - TITLE
--
multipleBooks :: Section -> Parser BookList
multipleBooks sec = do
    auth <- restOfLine
    Set.fromList . map ((flip.flip Book) auth sec) <$> readTitles
  where
    readTitles = do indentSize <- length <$> many1 space
                    readItem `sepBy` count indentSize space
    readItem   = char '-' >> skipSpace >> restOfLine

-- | Reads a section
readBookSection :: Parser BookList
readBookSection = mconcat <$> (section >>= many' . entry) <* skipLines

document :: Parser BookList
document = option () (void comment) >> skipLines >>
           mconcat <$> many1 readBookSection

readBookList :: Text -> Either String BookList
readBookList = parseOnly document
