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
import           Control.Monad.Reader           ( MonadReader )

importM
  :: (MonadIO m, MonadReader env m, MonadError Text m, CanLog env)
  => [Album]
  -> m ()
importM albums = do
  report_ "Importing albums"
  either throwError (void . sh . beetImport) textPaths
  report_ "Finished importing albums"
 where
  albumPaths = P.map baseDir albums
  textPaths  = mapM toText albumPaths

beetImport :: [Text] -> Shell Line
beetImport paths = inproc "beet" ("import" : paths) (pure mempty)

updateM :: (MonadReader env m, CanLog env, MonadIO m) => m ()
updateM = do
  report_ "Updating beets"
  result <- Turtle.fold beetUpdate Fold.list
  mapM_ report_ result

beetUpdate :: Shell Text
beetUpdate = either whocares lineToText <$> doUpdate
  where doUpdate = inprocWithErr "beet" ["update"] (pure mempty)

whocares :: Line -> Text
whocares errLine = dismissal `T.append` lineToText errLine
  where dismissal = "beets says it failed, but it's probably fine"
