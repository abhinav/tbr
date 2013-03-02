module TBR.Util
    ( tokens
    , putLn
    , CounterT
    , runCounterT
    , counter
    ) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Char              (isAlphaNum, isSpace)
import qualified Data.Text.IO           as TIO
import           Control.Applicative
import           Control.Monad.State    (StateT, evalStateT, get, put)
import           Control.Monad.IO.Class

-- | Splits the given string into tokens that contain only alphanumeric
-- characters.
tokens :: Text -> [Text]
tokens = T.split isSpace . clean
    where clean   = T.toLower . T.filter isValid
          isValid = (||) <$> isSpace <*> isAlphaNum

-- | A version of @Data.Text.IO.putStrLn@ that runs in any monad capable of
-- lifting IO operations.
putLn :: MonadIO m => Text -> m ()
putLn = liftIO . TIO.putStrLn

-- | A monad transformer to maintain a running count of some kind.
type CounterT = StateT Integer

-- | Get the current value of the counter and increment it.
counter :: Monad m => CounterT m Integer
counter = do
    value <- get
    put (value + 1)
    return value

-- | Run the counter. Counting starts at 1.
runCounterT :: Monad m => CounterT m a -> m a
runCounterT = flip evalStateT 1
