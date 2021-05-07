{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                , MonadError
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader
                                                , asks
                                                )
import           Data.Either                    ( fromRight )
import           Data.Hashable                  ( Hashable
                                                , hashWithSalt
                                                )
import           Data.Text                     as T
import           Prelude                 hiding ( FilePath )
import           Turtle

newtype App a = App
  { runApp :: ReaderT Env (ExceptT Text IO) a
  } deriving (Monad, Functor, Applicative, MonadReader Env, MonadIO, MonadError Text)

data Env = Env
  { config  :: Config
  , logFunc :: Text -> IO ()
  }

class Monad m => HasConfig_ m where
  getConfig_ :: m Config

instance HasConfig_ App where
  getConfig_ = asks config

class Monad m => Logs m where
  report :: Text -> m ()

instance Logs App where
  report text = do
    logWith <- asks logFunc
    liftIO . logWith $ text

data Config = Config
  { musicDir          :: MusicDir
  , archiveOptions    :: ArchiveOptions
  , conversionOptions :: ConversionOptions
  }

data Album = Album
  { relativePath :: FilePath
  , absolutePath :: FilePath
  , albumName    :: Text
  , artistName   :: Text
  , songs        :: [Song]
  }
  deriving Show

artistAlbum :: Album -> Text
artistAlbum album = T.unwords [artistName album, "-", albumName album]

instance Hashable FilePath where
  hashWithSalt salt filePath = hashWithSalt salt pathTxt
    where pathTxt = fromRight "" . toText $ filePath

newtype Song = Song
  { songPath :: FilePath
  } deriving (Show)

newtype MusicDir = MusicDir
  { unMusicDir :: FilePath
  } deriving (Show)

newtype ArchiveDir = ArchiveDir { unArchiveDir :: FilePath }
  deriving (Show)

data ArchiveOptions
  = ZipArchive ArchiveDir
  | MoveArchive ArchiveDir
  | NoArchive
  deriving (Show)

data ConvertedSong = ConvertedSong
  { originalSong  :: Song
  , convertedSong :: Song
  }
  deriving Show

newtype Bitrate = Bitrate Int
  deriving Show

data ConversionOptions = ConversionOptions
  { bitrate :: Bitrate
  }
  deriving Show

_toText path = fromRight (T.pack $ show path) $ toText path

maybeToErr :: MonadError Text m => Text -> Maybe a -> m a
maybeToErr msg = maybe (throwError msg) pure
