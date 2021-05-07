{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Archive where

import           Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe                     ( fromMaybe )
import           Data.Either
import           Data.Functor                   ( ($>) )
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

type Archiver m = Album -> m (Either Text ())

class CanArchive m where
  getArchiver :: Archiver m
  getArchiveStatus :: m Text

instance CanArchive App where
  getArchiver albums = do
    opts <- asks $ archiveOptions . config
    archiverFromOptions opts $ albums
  getArchiveStatus = asks (archiveStatus . archiveOptions . config)

data ArchiveTarget = ArchiveTarget
  { source :: Text
  , dest   :: Text
  }

archiveM
  :: (HasConfig_ m, MonadError Text m, CanArchive m) => [Album] -> m [Album]
archiveM = sequence . P.map (archiveOne getArchiver)

archiveStatus :: ArchiveOptions -> Text
archiveStatus NoArchive = "Skipping archiving"
archiveStatus (MoveArchive (ArchiveDir path)) =
  T.unwords ["Moving albums to", _toText $ path]
archiveStatus (ZipArchive (ArchiveDir path)) =
  T.unwords ["Zipping albums to", _toText $ path]

archiveOne :: (MonadError Text m) => Archiver m -> Album -> m Album
archiveOne archiver album = do
  result <- archiver album
  either throwError pure $ result $> album

archiverFromOptions :: ArchiveOptions -> Album -> App (Either Text ())
archiverFromOptions NoArchive                = const . pure . pure $ ()
archiverFromOptions (ZipArchive  archiveDir) = archiveZip archiveDir
archiverFromOptions (MoveArchive archiveDir) = archiveMove archiveDir

archiveZip :: ArchiveDir -> Album -> App (Either Text ())
archiveZip archive album = do
  target <- either throwError pure $ mkZipTarget archive album
  checkTargetDoesNotExist target (single $ zap target)

mkZipTarget :: ArchiveDir -> Album -> Either Text ArchiveTarget
mkZipTarget (ArchiveDir archDir) album@Album { absolutePath = albumDir } = do
  let zipName    = fromText $ T.concat [artistAlbum album, ".7z"]
      outputFile = archDir </> zipName
  source <- toText albumDir
  dest   <- toText outputFile
  pure $ ArchiveTarget source dest

zap :: ArchiveTarget -> Shell (Either Text ())
zap ArchiveTarget { source = folderToZip, dest = zipDest } = do
  result <- inprocWithErr "7z" ["a", zipDest, folderToZip] (pure mempty)
  return $ either (Left . linesToText . pure) (const $ Right ()) result

archiveMove :: ArchiveDir -> Album -> App (Either Text ())
archiveMove archive album = do
  target <- either throwError pure $ mkMoveTarget archive album
  checkTargetDoesNotExist target $ do
    mktree . fromText $ dest target
    single $ rsync target

mkMoveTarget :: ArchiveDir -> Album -> Either Text ArchiveTarget
mkMoveTarget (ArchiveDir archDir) Album { absolutePath, relativePath } = do
  source <- toText absolutePath
  dest   <- toText $ archDir </> relativePath
  pure $ ArchiveTarget source dest

rsync :: ArchiveTarget -> Shell (Either Text ())
rsync (ArchiveTarget source dest) = do
  result <- proc "rsync" [source, "-r", "--append-verify", dest] (pure mempty)
  case result of
    ExitSuccess   -> return $ Right ()
    ExitFailure _ -> return $ syncFailureMsg
 where
  syncFailureMsg =
    Left $ T.unwords ["Failed to sync album from", source, " to ", dest]

checkTargetDoesNotExist
  :: ArchiveTarget -> App (Either Text ()) -> App (Either Text ())
checkTargetDoesNotExist ArchiveTarget { dest } continuation = do
  doesExist <- testpath $ fromText dest
  if not doesExist then continuation else confirmOverwrite
 where
  confirmOverwrite = do
    confirmation <- readResponse dest
    if confirmation
      then continuation
      else do
        report $ T.unwords ["Ok, skipping destination:", dest]
        pure . pure $ ()

readResponse :: Text -> App Bool
readResponse dest = do
  report
    $ T.unwords ["Continue with archiving and possible overrwrite", dest, "?"]
  rawResponse <- readline
  let response = lineToText $ fromMaybe "" rawResponse
  case response of
    "y" -> pure True
    "Y" -> pure True
    ""  -> pure True
    "n" -> pure False
    "N" -> pure False
    _   -> throwError "Unrecognized response, bombing out."
