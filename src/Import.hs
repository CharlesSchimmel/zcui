module Import
  ( importM
  , updateM
  )
where

import           Types

import qualified Control.Foldl                 as Fold
import           Control.Monad
import           Control.Monad.Except
import           Data.Either
import           Data.Functor
import qualified Data.Text                     as T
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

class CanImport m where
  importAlbums :: [Album] -> m ()

instance CanImport App where
  importAlbums albums = do
    paths <- either throwError pure $ mapM toText $ P.map absolutePath albums
    void . sh $ beetImport paths

importM :: (CanImport m, MonadError Text m, Logs m) => [Album] -> m ()
importM albums = do
  report "Importing albums"
  importAlbums albums
  report "Finished importing albums"

beetImport :: [Text] -> Shell Line
beetImport paths = inproc "beet" ("import" : paths) (pure mempty)

class CanUpdate m where
  updateLibrary :: m [Text]

instance CanUpdate App where
  updateLibrary = Turtle.reduce Fold.list beetUpdate

updateM :: (CanUpdate m, Logs m) => m ()
updateM = do
  result <- updateLibrary
  mapM_ report result

beetUpdate :: Shell Text
beetUpdate = either whocares lineToText <$> doUpdate
  where doUpdate = inprocWithErr "beet" ["update"] (pure mempty)

whocares :: Line -> Text
whocares errLine = dismissal `T.append` lineToText errLine
  where dismissal = "beets says it failed, but it's probably fine"
