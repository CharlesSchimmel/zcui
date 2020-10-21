module Import (importM, updateM) where

import Types

import qualified Control.Foldl as Fold
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either
import Data.Functor
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
  report "Importing albums" <*
  case textPaths of
    Left l -> throwError l
    Right paths -> void . sh $ beetImport paths
  where
    albumPaths = P.map baseDir albums
    textPaths = mapM toText albumPaths

beetImport :: [Text] -> Shell Line
beetImport paths = inproc "beet" ("import" : paths) (pure mempty)

updateM :: App ()
updateM = do
  report "Updating beets"
  result <- Turtle.fold beetUpdate Fold.list
  mapM_ report result

beetUpdate :: Shell Text
beetUpdate = either whocares lineToText <$> doUpdate
  where
    doUpdate = inprocWithErr "beet" ["update"] (pure mempty)

whocares :: Line -> Text
whocares errLine = dismissal `T.append` lineToText errLine
  where
    dismissal = "beets says it failed, but it's probably fine"
