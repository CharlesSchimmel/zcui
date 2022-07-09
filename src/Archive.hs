{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Archive
    ( CanArchive(..)
    , archiveM
    ) where

import           Types
import           Util

import qualified Control.Foldl                 as Fold
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except     ( except )
import           Data.Either
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

newtype ArchiveApp a = ArchiveApp
    { runArchiveApp :: ExceptT ArchiveFailure App a
    } deriving (Monad, Functor, Applicative, MonadReader Env, MonadIO, MonadError ArchiveFailure)

instance Logs ArchiveApp where
    report_ text = do
        logWith <- asks logFunc
        liftIO . logWith $ text

class CanTestPath m where
  pathExists :: FilePath -> m Bool

instance CanTestPath ArchiveApp where
    pathExists = testpath

class CanGetResponse m where
  getResponse :: Text -> m Text

instance CanGetResponse ArchiveApp where
    getResponse prompt = do
        report_ prompt
        rawResponse <- readline
        pure . lineToText . fromMaybe "" $ rawResponse

runArchiveInApp :: ArchiveApp a -> App (Either ArchiveFailure a)
runArchiveInApp = runExceptT . runArchiveApp

type Archiver m = Album -> m (Either ArchiveFailure ())

class MonadError Text m => CanArchive m where
  getArchiver :: Album -> m (Either ArchiveFailure ())
  getArchiveStatus :: m Text

instance CanArchive App where
    getArchiver album = do
        report . T.unwords $ ["Archiving", artistAlbum album]
        conf <- asks config
        let isDryRun = dryRun conf
            archiver = archiverFromOptions (archiveOptions conf)
        if isDryRun then pure . pure $ () else archiver album
    getArchiveStatus = asks (archiveStatus . archiveOptions . config)

data ArchiveTarget = ArchiveTarget
    { source :: Text
    , dest   :: Text
    }

data ArchiveFailure = Skipped | Error Text

archiveM
    :: (MonadError Text m, CanArchive m, Monad m, Logs m)
    => [Album]
    -> m [Album]
archiveM albums = do
    results <- P.mapM (\album -> (album, ) <$> getArchiver album) albums
    let (fails, movedOrSkippedAlbums) =
            partitionEithers $ P.map recoverSkips results
    tellArchiveFailures fails
    pure movedOrSkippedAlbums
  where
    recoverSkips (album, Left (Error msg)) = Left (album, msg)
    recoverSkips (album, _               ) = Right album

tellArchiveFailures :: Logs m => [(Album, Text)] -> m ()
tellArchiveFailures fails = do
    if P.null fails
        then pure ()
        else
            report
            . T.unwords
            . P.map T.pack
            $ [show $ P.length fails, "albums could not be archived"]
    let reasons    = P.map (uncurry tellFailure) fails
        withIndent = P.map (T.append "\t") reasons
    report . T.unlines $ withIndent

tellFailure :: Album -> Text -> Text
tellFailure album reason =
    T.unwords [artistAlbum album, "failed with reason:", reason]

archiveStatus :: ArchiveOptions -> Text
archiveStatus NoArchive = "Skipping archiving"
archiveStatus (MoveArchive (ArchiveDir path)) =
    T.unwords ["Moving albums to", _toText path]
archiveStatus (ZipArchive (ArchiveDir path)) =
    T.unwords ["Zipping albums to", _toText path]

archiverFromOptions
    :: ArchiveOptions -> Album -> App (Either ArchiveFailure ())
archiverFromOptions NoArchive = const . pure . pure $ ()
archiverFromOptions (ZipArchive archiveDir) =
    runArchiveInApp . archiveZip archiveDir
archiverFromOptions (MoveArchive archiveDir) =
    runArchiveInApp . archiveMove archiveDir

archiveZip :: ArchiveDir -> Album -> ArchiveApp ()
archiveZip archive album = do
    target@ArchiveTarget { dest } <- liftEither . mapLeft Error $ mkZipTarget
        archive
        album
    liftEither =<< checkOverwrite dest
    liftEither . biMap (Error . T.unwords) (const ()) =<< reduce
        collectEithers
        (zap target)

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

archiveMove :: ArchiveDir -> Album -> ArchiveApp ()
archiveMove archive album = do
    target@ArchiveTarget { dest } <- liftEither . mapLeft Error $ mkMoveTarget
        archive
        album
    -- break if Skipped
    liftEither =<< checkOverwrite dest
    mktree $ fromText dest
    liftEither . mapLeft Error =<< (liftIO . single $ rsync target)

mkMoveTarget :: ArchiveDir -> Album -> Either Text ArchiveTarget
mkMoveTarget (ArchiveDir archDir) album@Album { absolutePath, relativePath } =
    do
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
        Left $ T.unwords
            ["Rsync failed to sync album from", source, " to ", dest]

checkOverwrite
    :: (Logs m, CanTestPath m, CanGetResponse m)
    => Text
    -> m (Either ArchiveFailure ())
checkOverwrite dest = do
    doesExist <- pathExists $ fromText dest
    if not doesExist then pure . pure $ () else parseResponse dest

parseResponse
    :: (Logs m, CanGetResponse m) => Text -> m (Either ArchiveFailure ())
parseResponse dest = do
    response <- getResponse
        $ T.concat ["Files exist at: '", dest, "' Overwrite? [Y/n] "]
    case response of
        "y" -> pure $ Right ()
        "Y" -> pure $ Right ()
        ""  -> pure $ Right ()
        "n" -> pure $ Left Skipped
        "N" -> pure $ Left Skipped
        _   -> do
            report "Unrecognized response."
            parseResponse dest
