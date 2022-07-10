module Class where

import           Control.Monad.Reader           ( asks )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                     as T
import           Prelude                 hiding ( FilePath )
import           Turtle
import           Types

class Prompts m where
  getResponse :: Text -> m Text

class TestsPath m where
  pathExists :: FilePath -> m Bool

instance Prompts App where
    getResponse prompt = do
        report_ prompt
        rawResponse <- readline
        pure . lineToText . fromMaybe "" $ rawResponse

instance TestsPath App where
    pathExists = testpath

class Monad m => HasConfig_ m where
  getConfig_ :: m Config

instance HasConfig_ App where
    getConfig_ = asks config

class Monad m => Logs m where
  report_ :: Text -> m ()
  report :: Text -> m ()
  report text = report_ $ T.unlines [text]

instance Logs App where
    report_ text = do
        logWith <- asks logFunc
        liftIO . logWith . T.unlines $ [text]

