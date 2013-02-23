module TBR.Util (tokens, putLn) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Char              (isAlphaNum, isSpace)
import qualified Data.Text.IO           as TIO
import           Control.Applicative
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


