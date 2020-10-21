{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Types where

import Control.Monad.Except (ExceptT, throwError, MonadError)
import Control.Monad.Reader (ReaderT, MonadReader)
import Data.Either (fromRight)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Text as T
import Filesystem.Path.CurrentOS (FilePath)
import GHC.Generics (Generic)
import Prelude hiding (FilePath)
import Turtle

data Album = Album
  { baseDir :: FilePath
  , albumName :: Text
  , artistName :: Text
  , songs :: [Song]
  } deriving (Show)

artistAlbumPath :: Album -> FilePath
artistAlbumPath (Album _ album artist _) = fromText artist </> fromText album

artistAlbum :: Album -> Text
artistAlbum album = T.unwords [artistName album, "-", albumName album]

instance Hashable FilePath where
  hashWithSalt salt filePath = hashWithSalt salt pathTxt
    where
      pathTxt = fromRight "" . toText $ filePath

newtype Song = Song
  { songPath :: FilePath
  } deriving (Show)

newtype MusicDir = MusicDir
  { unMusicDir :: FilePath
  } deriving (Show)

mkMusicDir :: String -> MusicDir
mkMusicDir = MusicDir . fromText . T.pack

mkArchiveDir :: String -> ArchiveDir
mkArchiveDir = ArchiveDir . fromText . T.pack

newtype ArchiveDir =
  ArchiveDir FilePath
  deriving (Show)

data ArchiveOptions
  = ZipArchive ArchiveDir
  | MoveArchive ArchiveDir
  | NoArchive
  deriving (Show)

data Options = Options
  { musicDir :: MusicDir
  , archiveOptions :: ArchiveOptions
  } deriving (Show)

type AppConfig = MonadReader Options

newtype App a = App
  { runApp :: ReaderT Options (ExceptT Text IO) a
  } deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError Text)

data ConvertedSong = ConvertedSong
  { originalSong :: Song
  , convertedSong :: Song
  } deriving (Show)

devNull :: Shell Line -> Shell ()
devNull = output "/dev/null"

_toText = fromRight "" . toText

report :: Text -> App ()
report = liftIO . putStrLn . T.unpack

maybeToErr :: Text -> Maybe a -> App a
maybeToErr msg = maybe (throwError msg) pure
