
module Zcui.Types where

import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
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

data Config = Config
    { musicDir          :: MusicDir
    , archiveOptions    :: ArchiveOptions
    , conversionOptions :: ConversionOptions
    , dryRun            :: Bool
    }

data Album = Album
    { relativePath :: FilePath
    , absolutePath :: FilePath
    , albumName    :: Text
    , artistName   :: Text
    , songs        :: [Song]
    }
    deriving (Show, Eq)

artistAlbum :: Album -> Text
artistAlbum album = T.unwords [artistName album, "-", albumName album]

instance Hashable FilePath where
    hashWithSalt salt filePath = hashWithSalt salt pathTxt
        where pathTxt = fromRight "" . toText $ filePath

newtype Song = Song
  { songPath :: FilePath
  } deriving (Show, Eq, Ord)

songFileName :: Song -> Text
songFileName Song { songPath } =
    fromRight "(oops, could not toText song path)" (toText $ filename songPath)

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

newtype Bitrate = Bitrate Int
  deriving Show

data ConversionOptions = ConversionOptions
    { bitrate :: Bitrate
    }
    deriving Show
