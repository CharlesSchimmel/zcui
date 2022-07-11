module Zcui.Archive.Internal where

import           Zcui.Archive.Types
import           Zcui.Class
import           Zcui.Types
import           Zcui.Util

import           Control.Monad.Reader           ( asks )
import           Data.Bifunctor                 ( bimap )
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Text                      ( Text(..) )
import           Prelude                 hiding ( FilePath )
import           Turtle

toTextTarget :: Album -> FilePath -> Either Text ArchiveTarget
toTextTarget Album { absolutePath } dest = do
    sourceText <- toText absolutePath
    destText   <- toText dest
    Right $ ArchiveTarget sourceText destText

mkZipDest :: ArchiveDir -> Album -> FilePath
mkZipDest (ArchiveDir archDir) album@Album { absolutePath } =
    let zipName = fromText $ T.concat [artistAlbum album, ".7z"]
    in  archDir </> zipName

mkMoveTarget :: ArchiveDir -> Album -> FilePath
mkMoveTarget (ArchiveDir archDir) album@Album { absolutePath, relativePath } =
    archDir </> relativePath

doZip :: MonadIO m => Archiver m
doZip target =
    biMap T.unwords (const ()) <$> reduce collectEithers (zap target)

zap :: ArchiveTarget -> Shell (Either Text ())
zap ArchiveTarget { source = folderToZip, dest = zipDest } = do
    result <- inprocWithErr "7z" ["a", zipDest, folderToZip] (pure mempty)
    return $ either (Left . linesToText . pure) (const $ Right ()) result

doMove :: MonadIO m => Archiver m
doMove target@ArchiveTarget { dest } = do
    mktree $ fromText dest
    liftIO . single $ rsync target

rsync :: ArchiveTarget -> Shell (Either Text ())
rsync (ArchiveTarget source dest) = do
    result <- proc "rsync" [source, "-r", "--append-verify", dest] (pure mempty)
    case result of
        ExitSuccess -> return $ Right ()
        ExitFailure _ ->
            return
                . Left
                . T.unwords
                $ ["Rsync failed to sync from", source, " to ", dest]

