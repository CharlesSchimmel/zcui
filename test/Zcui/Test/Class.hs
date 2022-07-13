module Zcui.Test.Class where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Text
import           Zcui.Class                     ( Logs(..) )

newtype MockM env a = MockM
    { runEnv :: ReaderT env (ExceptT Text Identity) a
    } deriving (Monad, Functor, Applicative, MonadError Text, MonadReader env)

instance Logs (MockM env) where
    report_ msg = pure ()

runMock env = runIdentity . runExceptT . flip runReaderT env . runEnv
