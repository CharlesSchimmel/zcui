{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Archive where

import           Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Either
import           Data.Maybe
import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

type Archiver m = RelativeAlbum -> m (Either T.Text Album)

class CanArchive m where
  getArchiver :: Archiver m
  getArchiveStatus :: m Text

instance CanArchive App where
  getArchiver albums = do
    opts <- asks $ archiveOptions . config
    liftIO . mkArchiver opts $ albums
  getArchiveStatus = asks (archiveStatus . archiveOptions . config)

archiveM
  :: (HasConfig_ m, MonadError Text m, CanArchive m) => [Album] -> m [Album]
archiveM albums = do
  musicDir' <- musicDir <$> getConfig_
  relAlbums <- pathRelativeAlbums musicDir' albums
  archiveMany getArchiver relAlbums

archiveStatus :: ArchiveOptions -> Text
archiveStatus NoArchive                       = "Skipping archiving"
archiveStatus (MoveArchive (ArchiveDir path)) = T.unwords
  ["Moving albums to", fromRight "could not parse archive path" . toText $ path]
archiveStatus (ZipArchive (ArchiveDir path)) = T.unwords
  [ "Zipping albums to"
  , fromRight "could not parse archive path" . toText $ path
  ]

pathRelativeAlbums
  :: (MonadError Text m) => MusicDir -> [Album] -> m [RelativeAlbum]
pathRelativeAlbums musicDir albums = do
  maybeToErr "Could not construct destination path"
    $ P.mapM (mkRelativeAlbum musicDir) albums

mkRelativeAlbum :: MusicDir -> Album -> Maybe RelativeAlbum
mkRelativeAlbum (MusicDir musicDir) album =
  RelativeAlbum album <$> stripPrefix (musicDir </> mempty) (baseDir album)

archiveOne :: (MonadError Text m) => Archiver m -> RelativeAlbum -> m Album
archiveOne archiver album = do
  result <- archiver album
  either throwError pure result

archiveMany
  :: (MonadError Text m) => Archiver m -> [RelativeAlbum] -> m [Album]
archiveMany archiver albums = sequence $ P.map (archiveOne archiver) albums

mkArchiver :: MonadIO m => ArchiveOptions -> Archiver m
mkArchiver opts albums = single $ archiverFromOptions opts albums

archiverFromOptions
  :: ArchiveOptions -> RelativeAlbum -> Shell (Either Text Album)
archiverFromOptions NoArchive                = const (select [])
archiverFromOptions (ZipArchive  archiveDir) = archiveZip' archiveDir . _album
archiverFromOptions (MoveArchive archiveDir) = archiveMove' archiveDir

archiveZip' :: ArchiveDir -> Album -> Shell (Either Text Album)
archiveZip' archDir album = archiveZip archDir album <&> ($> album)

archiveMove' :: ArchiveDir -> RelativeAlbum -> Shell (Either Text Album)
archiveMove' archDir album = archiveMove archDir album <&> ($> (_album album))

archiveZip :: ArchiveDir -> Album -> Shell (Either Text ())
archiveZip archive album@Album {..} = zap (buildZipPath archive album) baseDir

buildZipPath :: ArchiveDir -> Album -> FilePath
buildZipPath (ArchiveDir dir) album = dir </> fromText (buildZipName album)

buildZipName :: Album -> Text
buildZipName album = T.concat [artistAlbum album, ".7z"]

zap :: FilePath -> FilePath -> Shell (Either Text ())
zap zipFilePath folderToZip = either (pure . Left) id
  $ liftM2 zap_ (toText zipFilePath) (toText folderToZip)
 where
  zap_ zipFile zipTarget =
    either (Left . linesToText . pure) (const $ Right ())
      <$> inprocWithErr "7z" ["a", zipFile, zipTarget] (pure mempty)

archiveMove :: ArchiveDir -> RelativeAlbum -> Shell (Either Text ())
archiveMove (ArchiveDir storageBase) album = do
  let storageAlbumPath = storageBase </> relativePath album
  mktree storageAlbumPath
  rsync (baseDir . _album $ album) storageAlbumPath

rsync :: FilePath -> FilePath -> Shell (Either Text ())
rsync source dest = either (pure . Left) id
  $ liftM2 rsync_ (toText source) (toText dest)
 where
  rsync_ :: Text -> Text -> Shell (Either Text ())
  rsync_ s d = do
    result <- proc "rsync" [s, "-r", "--append-verify", d] (pure mempty)
    case result of
      ExitSuccess   -> return $ Right ()
      ExitFailure _ -> return $ syncFailureMsg source dest
  syncFailureMsg source dest = Left . T.concat $ P.map
    T.pack
    ["Failed to sync album from ", show source, " to ", show dest]


