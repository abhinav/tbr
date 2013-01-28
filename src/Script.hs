module Script (ScriptT, runScriptT, scriptIO) where

import System.Exit (exitFailure)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Error (scriptIO, EitherT, runEitherT, errLn)

type ScriptT m = EitherT String m

runScriptT :: MonadIO m => ScriptT m a -> m a
runScriptT s = do
    ea <- runEitherT s
    case ea of
        Left  e -> liftIO (errLn e >> exitFailure)
        Right a -> return a

