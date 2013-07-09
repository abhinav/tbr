{-# LANGUAGE OverloadedStrings #-}
module TBR.Util
    ( tokens
    , tokenMatch
    , putLn
    , puts
    , CounterT
    , runCounterT
    , counter
    , capitalize
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.State    (StateT, evalStateT, get, put)
import           Data.Char              (isAlphaNum)
import           Data.Function          (on)
import           Data.List              (isInfixOf)
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as TIO

-- | Splits the given string into tokens that contain only alphanumeric
-- characters.
tokens :: Text -> [Text]
tokens = filter (not . Text.null)
       . Text.split (not . isAlphaNum)
       . Text.toLower

-- | @l `tokenMatch` r@ checks if all tokens of @l@ are in @r@.
tokenMatch :: Text -> Text -> Bool
tokenMatch = isInfixOf `on` tokens

-- | A version of @Data.Text.IO.putStrLn@ that runs in any monad capable of
-- lifting IO operations.
putLn :: MonadIO m => Text -> m ()
putLn = liftIO . TIO.putStrLn

puts :: MonadIO m => Text -> m ()
puts = liftIO . TIO.putStr

-- | A monad transformer to maintain a running count of some kind.
type CounterT = StateT Int

-- | Get the current value of the counter and increment it.
counter :: Monad m => CounterT m Int
counter = do
    value <- get
    put $! value + 1
    return value

-- | Run the counter. Counting starts at 1.
runCounterT :: Monad m => CounterT m a -> m a
runCounterT = flip evalStateT 1

capitalize :: Text -> Text
capitalize = Text.intercalate " " . map go . Text.splitOn " "
    where go = (Text.toUpper . Text.take 1) <> Text.drop 1
