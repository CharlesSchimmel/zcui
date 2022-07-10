module Import
    ( importM
    , updateM
    ) where

import           Class
import           Types

import qualified Control.Foldl                 as Fold
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader           ( asks )
import           Data.Either
import           Data.Functor
import qualified Data.Text                     as T
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

class Imports m where
  importAlbums :: [Album] -> m ()

instance Imports App where
    importAlbums albums = do
        isDryRun <- asks $ dryRun . config
        if isDryRun
            then pure ()
            else do
                paths <- liftEither $ mapM (toText . absolutePath) albums
                void . sh $ beetImport paths

class Updates m where
  updateLibrary :: m [Text]

instance Updates App where
    updateLibrary = do
        isDryRun <- asks $ dryRun . config
        if isDryRun then pure [] else Turtle.reduce Fold.list beetUpdate

importM :: (Imports m, MonadError Text m, Logs m) => [Album] -> m ()
importM albums = do
    report "Importing albums"
    importAlbums albums
    report "Finished importing albums"

beetImport :: [Text] -> Shell Line
beetImport paths = inproc "beet" ("import" : paths) (pure mempty)

updateM :: (Monad m, Updates m, Logs m) => m ()
updateM = do
    result <- updateLibrary
    mapM_ report result

beetUpdate :: Shell Text
beetUpdate = either whocares lineToText <$> doUpdate
    where doUpdate = inprocWithErr "beet" ["update"] (pure mempty)

whocares :: Line -> Text
whocares errLine = dismissal `T.append` lineToText errLine
    where dismissal = "beets says it failed, but it's probably fine"
