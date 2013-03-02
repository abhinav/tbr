{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module TBR.Writer (writeDocument) where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Text              as T
import           Data.Text.Lazy         (Text)
import           Data.Text.Lazy.Builder
import           TBR.Types

type TextBuilder = Writer Builder

runTextBuilder :: TextBuilder a -> Text
runTextBuilder = toLazyText . execWriter

tellText :: T.Text -> TextBuilder ()
tellText = void . tell . fromText

type Counter m = StateT Integer m

runCounter :: Monad m => Counter m a -> m a
runCounter = flip evalStateT 1

next :: Monad m => Counter m Integer
next = do x <- get
          put (x + 1)
          return x

newln :: TextBuilder ()
newln = void $ tell "\n"

writeDocument :: Document -> Text
writeDocument = runTextBuilder . mapM_ writeBlocks

writeBlocks :: Block -> TextBuilder ()
writeBlocks Block{..} = void $
       writeHeader blockHeader >> newln
    >> runCounter (mapM_ (writeEntry numEntries) blockEntries)
    >> newln
  where numEntries = fromIntegral $ length blockEntries

writeHeader :: T.Text -> TextBuilder ()
writeHeader hdr = void $ tellText hdr      >> newln
               >> underline (T.length hdr) >> newln
  where underline n = tellText $ T.replicate (fromIntegral n) "="

writeEntry :: Integer -> Entry -> Counter TextBuilder ()
writeEntry _ (Entry _ []) = return ()
writeEntry total Entry{..} = do
    pos <- next
    lift $ do
        tell (fromString $ show pos) >> tell ". "
        tellText $ spaces (numLen total - numLen pos)
        case entryBooks of
            [b] -> tellText b >> tell " - " >> tellText entryAuthor >> newln
            bs  -> tellText entryAuthor >> newln
                >> mapM_ writeBook bs
  where
    numLen = length . show
    spaces = flip T.replicate " "

    writeBook :: T.Text -> TextBuilder ()
    writeBook b = void $ do
        tellText $ spaces (numLen total + 2)
        tell "-   "
        tellText b
        newln
