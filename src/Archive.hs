{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Archive where

import           Types
import           Util

import qualified Control.Foldl                 as Fold
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Either
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

type Archiver m = Album -> m (Either Text ())

class CanArchive m where
  getArchiver :: Album -> m (Either Text ())
  getArchiveStatus :: m Text

instance CanArchive App where
  getArchiver album = do
    report . T.unwords $ ["Archiving", artistAlbum album]
    conf <- asks config
    let isDryRun = dryRun conf
        archiver = archiverFromOptions $ archiveOptions conf
    if isDryRun then pure $ pure () else archiver album
  getArchiveStatus = asks (archiveStatus . archiveOptions . config)

data ArchiveTarget = ArchiveTarget
  { source :: Text
  , dest   :: Text
  }

data ArchiveFailure = ArchiveFailure Album Text

archiveM
  :: (MonadError Text m, CanArchive m, Monad m, Logs m) => [Album] -> m [Album]
archiveM albums = do
  results <- P.mapM (archiveOne getArchiver) albums
  let (lefts, rights) = partitionEithers results
  tellArchiveFailures lefts
  pure rights

tellArchiveFailures :: Logs m => [ArchiveFailure] -> m ()
tellArchiveFailures fails = do
  if P.null fails
    then pure ()
    else
      report
      . T.unwords
      . P.map T.pack
      $ [show $ P.length fails, "albums could not be archived"]
  let reasons    = P.map tellFailure fails
      withIndent = P.map (T.append "\t") reasons
  report . T.unlines $ withIndent

tellFailure :: ArchiveFailure -> Text
tellFailure (ArchiveFailure album reason) =
  T.unwords [artistAlbum album, "failed with reason:", reason]

archiveStatus :: ArchiveOptions -> Text
archiveStatus NoArchive = "Skipping archiving"
archiveStatus (MoveArchive (ArchiveDir path)) =
  T.unwords ["Moving albums to", _toText path]
archiveStatus (ZipArchive (ArchiveDir path)) =
  T.unwords ["Zipping albums to", _toText path]

archiveOne
  :: (Monad m) => Archiver m -> Album -> m (Either ArchiveFailure Album)
archiveOne archiver album =
  biMap (ArchiveFailure album) (const album) <$> archiver album

archiverFromOptions :: ArchiveOptions -> Album -> App (Either Text ())
archiverFromOptions NoArchive                = const . pure . pure $ ()
archiverFromOptions (ZipArchive  archiveDir) = archiveZip archiveDir
archiverFromOptions (MoveArchive archiveDir) = archiveMove archiveDir

archiveZip :: ArchiveDir -> Album -> App (Either Text ())
archiveZip archive album = do
  let targetResult = mkZipTarget archive album
  either (pure . Left) proceed targetResult
 where
  proceed target@ArchiveTarget { dest } = checkOverwrite dest $ do
    result <- reduce collectEithers $ zap target
    pure $ biMap T.unlines (const ()) result

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
  let targetResult = mkMoveTarget archive album
  either (pure . Left) proceed targetResult
 where
  proceed target@ArchiveTarget { dest } = checkOverwrite dest $ do
    mktree $ fromText dest
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
    ExitFailure _ -> return syncFailureMsg
 where
  syncFailureMsg =
    Left $ T.unwords ["Failed to sync album from", source, " to ", dest]

class CanTestPath m where
  pathExists :: FilePath -> m Bool

instance CanTestPath App where
  pathExists = testpath

class CanGetResponse m where
  getResponse :: Text -> m Text

instance CanGetResponse App where
  getResponse prompt = do
    report prompt
    rawResponse <- readline
    pure . lineToText . fromMaybe "" $ rawResponse

checkOverwrite
  :: (Logs m, CanTestPath m, MonadError Text m, CanGetResponse m)
  => Text
  -> m (Either Text ())
  -> m (Either Text ())
checkOverwrite dest continuation = do
  doesExist <- pathExists $ fromText dest
  if not doesExist
    then continuation
    else do
      confirmation <- parseResponse dest
      if confirmation
        then continuation
        else do
          report $ T.unwords ["Ok, skipping destination:", dest]
          pure . Left $ T.unwords [dest, "was skipped"]

parseResponse
  :: (CanGetResponse m, MonadError Text m, Logs m) => Text -> m Bool
parseResponse dest = do
  response <- getResponse
    $ T.unwords ["Continue with archiving and possible overwrite", dest, "?"]
  case response of
    "y" -> pure True
    "Y" -> pure True
    ""  -> pure True
    "n" -> pure False
    "N" -> pure False
    _   -> throwError "Unrecognized response, bombing out."
