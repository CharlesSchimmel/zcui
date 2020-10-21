module Archive (archivesM) where 

import Types

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either
import Data.HashMap as HM
import Data.Hashable
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace
import Filesystem.Path
import Filesystem.Path.CurrentOS
import Prelude as P hiding (FilePath)
import Turtle

data RelativeAlbum = RelativeAlbum
  { _album :: Album
  , relativePath :: FilePath
  } deriving (Show)

archivesM :: [Album] -> App [Album]
archivesM albums = do
  relAlbums <- pathRelativeAlbums albums
  opts <- asks archiveOptions
  report . archiveStatus $ opts
  _ <- sh $ archive opts relAlbums
  pure albums

archiveStatus :: ArchiveOptions -> Text
archiveStatus NoArchive = "Skipping archiving"
archiveStatus (MoveArchive (ArchiveDir path)) =
  T.unwords
    [ "Moving albums to"
    , fromRight "could not parse archive path" . toText $ path
    ]
archiveStatus (ZipArchive (ArchiveDir path)) =
  T.unwords
    [ "Zipping albums to"
    , fromRight "could not parse archive path" . toText $ path
    ]


pathRelativeAlbums :: [Album] -> App [RelativeAlbum]
pathRelativeAlbums albums = do
  musicDir <- unMusicDir <$> asks musicDir
  maybeToErr "Could not construct destination path" $
    P.mapM (mkRelativeAlbum musicDir) albums

mkRelativeAlbum :: FilePath -> Album -> Maybe RelativeAlbum
mkRelativeAlbum musicDir album =
  RelativeAlbum album <$> stripPrefix (musicDir </> mempty) (baseDir album)

archive :: ArchiveOptions -> [RelativeAlbum] -> Shell (Either Text [Album])
archive opt albums =
  fmap (const . P.map _album $ albums) <$> doArchive opt albums

doArchive :: ArchiveOptions -> [RelativeAlbum] -> Shell (Either Text ())
doArchive NoArchive = const . pure . pure $ ()
doArchive (ZipArchive archiveDir) = cat . P.map (archiveZip archiveDir . _album)
doArchive (MoveArchive archiveDir) = cat . P.map (archiveMove archiveDir)

archiveZip :: ArchiveDir -> Album -> Shell (Either Text ())
archiveZip (ArchiveDir archiveDir) (Album albumPath album artist _) =
  zap zipPath albumPath
  where
    zipName = T.concat [T.unwords [artist, "-", album], ".7z"]
    zipPath = archiveDir </> fromText zipName

zap :: FilePath -> FilePath -> Shell (Either Text ())
zap zipFilePath folderToZip =
  either (pure . Left) id $
  liftM2 zap_ (toText zipFilePath) (toText folderToZip)
  where
    zap_ zipFile zipTarget =
      either (Left . linesToText . pure) (const $ Right ()) <$>
      inprocWithErr "7z" ["a", zipFile, zipTarget] (pure mempty)

archiveMove :: ArchiveDir -> RelativeAlbum -> Shell (Either Text ())
archiveMove (ArchiveDir storageBase) album = do
  let storageAlbumPath = storageBase </> relativePath album
  mktree storageAlbumPath
  rsync (baseDir . _album $ album) storageAlbumPath

rsync :: FilePath -> FilePath -> Shell (Either Text ())
rsync source dest =
  either (pure . Left) id $ liftM2 rsync_ (toText source) (toText dest)
  where
    rsync_ :: Text -> Text -> Shell (Either Text ())
    rsync_ s d = do
      result <- proc "rsync" [s, "-r", "--append-verify", d] (pure mempty)
      case result of
        ExitSuccess -> return $ Right ()
        ExitFailure _ -> return $ syncFailureMsg source dest
    syncFailureMsg source dest =
      Left . T.concat $
      [ T.pack "Failed to sync album from "
      , _toText source
      , " to "
      , _toText dest
      ]
