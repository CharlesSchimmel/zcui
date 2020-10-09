module Import where

import Types

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either
import Data.HashMap as HM
import Data.Hashable
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace
import Filesystem.Path
import Filesystem.Path.CurrentOS
import Prelude as P hiding (FilePath)
import Turtle

importM :: [Album] -> App ()
importM albums =
  case textPaths of
    Left l -> throwError l
    Right paths -> void . sh $ beetImport paths
  where
    albumPaths = P.map baseDir albums
    textPaths = mapM toText albumPaths

beetImport :: [Text] -> Shell Line
beetImport paths = inproc "beet" ("import" : paths) (pure mempty)

beetUpdate :: Shell Line
beetUpdate = inproc "beet" ["update"] (pure mempty)
