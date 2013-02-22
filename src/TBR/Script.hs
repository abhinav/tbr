module TBR.Script (ScriptT, runScriptT, scriptIO) where

import           Data.Text              (Text, pack)
import           System.IO              (stderr)
import           System.Exit            (exitFailure)
import           Control.Monad.IO.Class
import qualified Control.Error as E
import qualified Data.Text.IO as TIO

type ScriptT m = E.EitherT Text m

scriptIO :: MonadIO m => IO a -> ScriptT m a
scriptIO a = E.catchT (E.scriptIO a)
                      (E.left . pack)

runScriptT :: MonadIO m => ScriptT m a -> m a
runScriptT s = do
    ea <- E.runEitherT s
    case ea of
        Left  e -> liftIO (errLn e >> exitFailure)
        Right a -> return a

errLn :: Text -> IO ()
errLn = TIO.hPutStrLn stderr
