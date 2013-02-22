{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Writer (writeDocument) where

import           Types
import qualified Data.Text                 as T
import           Data.Text.Lazy            (Text)
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Text.Lazy.Builder

type TextBuilder = Writer Builder ()

runTextBuilder :: TextBuilder -> Text
runTextBuilder = toLazyText . execWriter

tellText :: T.Text -> TextBuilder
tellText = tell . fromText

type Counter = StateT Integer

runCounter :: Monad m => Counter m a -> m a
runCounter = flip evalStateT 1

next :: Monad m => Counter m Integer
next = do x <- get
          put (x + 1)
          return x

newln :: TextBuilder
newln = tell "\n"

writeDocument :: Document -> Text
writeDocument = runTextBuilder . mapM_ writeBlocks . documentBlocks

writeBlocks :: Block -> TextBuilder
writeBlocks Block{..} = writeHeader blockHeader >> newln
                     >> mapM_ (writeEntry numEntries) blockEntries
    where numEntries = fromIntegral $ length blockEntries

writeHeader :: Header -> TextBuilder
writeHeader hdr = tellText hdr             >> newln
               >> underline (T.length hdr) >> newln
  where underline n = tellText $ T.replicate (fromIntegral n) "="

writeEntry :: Integer -> Entry -> TextBuilder
writeEntry _ (Entry _ []) = return ()
writeEntry total Entry{..} = runCounter $ do
    pos <- next
    lift $ do
        tell (fromString $ show pos) >> tell ". "
        tellText $ spaces (numLen total - numLen pos)
        case entryBooks of
            [b] -> tellText b >> tell " - " >> tellText entryAuthor
            bs  -> tellText entryAuthor >> tell " - "
                >> mapM_ writeBook bs
  where
    numLen = length . show
    spaces = flip T.replicate " "

    writeBook :: Book -> TextBuilder
    writeBook b = do
        tellText $ spaces (numLen total + 3)
        tell "- "
        tellText b

