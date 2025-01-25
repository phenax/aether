module Aether.Runtime.LangIO where

import Aether.Types
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadState, MonadTrans (lift))
import Data.Text.IO (hGetContents)
import System.Process (CreateProcess (..), StdStream (..))
import qualified System.Process as Proc

newtype LangIOT m a = LangIOT {runLangIOT :: m a}
  deriving (Functor, Applicative, Monad)

deriving instance (MonadState s m) => MonadState s (LangIOT m)

deriving instance (MonadError e m) => MonadError e (LangIOT m)

instance (MonadIO m) => MonadLangIO (LangIOT m) where
  putStringToScreen = lift . liftIO . putStr
  execCommand cmd args = lift . liftIO $ do
    let command =
          (Proc.proc cmd args)
            { close_fds = True,
              std_out = CreatePipe,
              std_err = CreatePipe
            }
    process <- Proc.createProcess command
    let (_, stdoutH, stderrH, procH) = process
    exitCode <- Proc.waitForProcess procH
    stdout <- maybe (pure "") hGetContents stdoutH
    stderr <- maybe (pure "") hGetContents stderrH
    Proc.cleanupProcess process
    pure (exitCode, stdout, stderr)

instance MonadTrans LangIOT where
  lift = LangIOT
