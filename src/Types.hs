{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Types
  ( App(..)
  , Options(..)
  , Album(..)
  , artistAlbum
  , artistAlbumPath
  , Song(..)
  , MusicDir(..)
  , ArchiveOptions(..)
  , ConvertedSong(..)
  , report
  , maybeToErr
  , ArchiveDir(..)
  , _toText
  )
where

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
  { runApp :: ReaderT Options (ExceptT Text IO) a
  } deriving (Monad, Functor, Applicative, AppConfig, MonadIO, MonadError Text)

class Monad m => Logs m where
  report :: Text -> m ()

instance Logs App where
  report = liftIO . putStrLn . T.unpack

instance Logs IO where
  report = putStrLn . T.unpack

type AppConfig = MonadReader Options

data Options = Options
  { musicDir       :: MusicDir
  , archiveOptions :: ArchiveOptions
  }
  deriving Show

data Album = Album
  { baseDir    :: FilePath
  , albumName  :: Text
  , artistName :: Text
  , songs      :: [Song]
  }
  deriving Show

artistAlbumPath :: Album -> FilePath
artistAlbumPath (Album _ album artist _) = fromText artist </> fromText album

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

_toText = fromRight "" . toText

-- report :: Text -> App ()
-- report = liftIO . putStrLn . T.unpack

maybeToErr :: MonadError Text m => Text -> Maybe a -> m a
maybeToErr msg = maybe (throwError msg) pure
