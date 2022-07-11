module Zcui.Class where

import           Zcui.Types

import           Control.Monad.Reader           ( asks )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                     as T
import           Prelude                 hiding ( FilePath )
import           Turtle

class Prompts m where
  getResponse :: Text -> m Text

class TestsPath m where
  pathExists :: FilePath -> m Bool

class Logs m where
  report_ :: Text -> m ()
  report :: Text -> m ()
  report text = report_ $ T.unlines [text]

instance Prompts App where
    getResponse prompt = do
        report_ prompt
        rawResponse <- readline
        pure . lineToText . fromMaybe "" $ rawResponse

instance TestsPath App where
    pathExists = testpath

instance Logs App where
    report_ text = do
        logWith <- asks logFunc
        liftIO . logWith . T.unlines $ [text]

checkOverwrite :: (Monad m, Logs m, TestsPath m, Prompts m) => Text -> m Bool
checkOverwrite dest = do
    doesExist <- pathExists $ fromText dest
    if not doesExist then pure True else parseResponse dest

parseResponse :: (Monad m, Logs m, Prompts m) => Text -> m Bool
parseResponse dest = do
    response <- getResponse
        $ T.concat ["Files exist at: '", dest, "' Overwrite? [Y/n] "]
    case response of
        "y" -> pure True
        "Y" -> pure True
        ""  -> pure True
        "n" -> pure False
        "N" -> pure False
        _   -> do
            report "Unrecognized response."
            parseResponse dest
