module Archive where

import           Types

import           Control.Monad.Except
import           Control.Monad
import           Control.Monad.Reader
import           Control.Foldl                  ( list )
import           Data.Either
import           Data.Maybe
import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

archivesM
  :: ( MonadIO m
     , MonadReader env m
     , HasConfig env
     , MonadError Text m
     , CanLog env
     )
  => [Album]
  -> m [Album]
archivesM albums = do
  archOpts <- asks (archiveOptions . getConfig)
  report_ . archiveStatus $ archOpts
  relAlbums <- pathRelativeAlbums albums
  archiveMany archOpts relAlbums

archiveStatus :: ArchiveOptions -> Text
archiveStatus NoArchive                       = "Skipping archiving"
archiveStatus (MoveArchive (ArchiveDir path)) = T.unwords
  ["Moving albums to", fromRight "could not parse archive path" . toText $ path]
archiveStatus (ZipArchive (ArchiveDir path)) = T.unwords
  [ "Zipping albums to"
  , fromRight "could not parse archive path" . toText $ path
  ]

pathRelativeAlbums
  :: (MonadError Text m, MonadReader env m, HasConfig env)
  => [Album]
  -> m [RelativeAlbum]
pathRelativeAlbums albums = do
  musicDir <- asks (musicDir . getConfig)
  maybeToErr "Could not construct destination path"
    $ P.mapM (mkRelativeAlbum musicDir) albums

mkRelativeAlbum :: MusicDir -> Album -> Maybe RelativeAlbum
mkRelativeAlbum (MusicDir musicDir) album =
  RelativeAlbum album <$> stripPrefix (musicDir </> mempty) (baseDir album)

archiveOne
  :: (MonadIO m, MonadError Text m)
  => ArchiveOptions
  -> RelativeAlbum
  -> m Album
archiveOne archOpts album = do
  result <- ((getArchiver archOpts) album)
  either throwError pure result

archiveMany
  :: (MonadIO m, MonadError Text m)
  => ArchiveOptions
  -> [RelativeAlbum]
  -> m [Album]
archiveMany archOpts albums = sequence $ P.map (archiveOne archOpts) albums

getArchiver :: MonadIO io => ArchiveOptions -> Archiver io
getArchiver opts albums = single $ doArchive opts albums

doArchive :: ArchiveOptions -> RelativeAlbum -> Shell (Either Text Album)
doArchive NoArchive                = const (select [])
doArchive (ZipArchive  archiveDir) = archiveZip' archiveDir . _album
doArchive (MoveArchive archiveDir) = archiveMove' archiveDir

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
  syncFailureMsg source dest =
    Left
      . T.concat
      $ [ T.pack "Failed to sync album from "
        , _toText source
        , " to "
        , _toText dest
        ]


