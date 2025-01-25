module Aether.Runtime.LangIO where

import Aether.Types
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadState, MonadTrans (lift))

newtype LangIOT m a = LangIOT {runLangIOT :: m a}
  deriving (Functor, Applicative, Monad)

deriving instance (MonadState s m) => MonadState s (LangIOT m)

deriving instance (MonadError e m) => MonadError e (LangIOT m)

instance (MonadIO m) => MonadLangIO (LangIOT m) where
  putStringToScreen = LangIOT . liftIO . putStr

instance MonadTrans LangIOT where
  lift = LangIOT
