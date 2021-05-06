module Delete
  ( deleteSongsM
  )
where
import           Types

import           Control.Monad.Reader           ( MonadReader )
import           Control.Monad
import           Control.Monad.Except
import           Data.Either
import           Data.Functor
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle                  hiding ( (<&>) )

class CanDelete m where
  delete :: Song -> m ()

instance CanDelete App where
  delete = deleteSong

deleteSongsM :: (Logs m, CanDelete m) => [Song] -> m ()
deleteSongsM songs = do
  void $ mapM delete songs

deleteSong :: (MonadIO m, Logs m) => Song -> m ()
deleteSong (Song songPath) = do
  report fileNameText
  sh $ rm songPath
 where
  fileNameText =
    fromRight "(oops, could not toText song path)" (toText $ filename songPath)
