module Delete
    ( deleteSongsM
    ) where
import           Class
import           Types

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Either
import           Data.Functor
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle                  hiding ( (<&>) )

class Deletes m where
  delete :: Song -> m ()

instance Deletes App where
    delete song = do
        isDryRun <- asks $ dryRun . config
        if isDryRun then pure () else deleteSong song

deleteSongsM :: (Monad m, Logs m, Deletes m) => [Song] -> m ()
deleteSongsM songs = do
    mapM_ delete songs

deleteSong :: (MonadIO m, Logs m) => Song -> m ()
deleteSong (Song songPath) = do
    sh $ rm songPath
