{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Archive
    ( archiveStatus
    , archiveM
    ) where

import           Types
import           Util

import qualified Control.Foldl                 as Fold
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except     ( except )
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Either
import           Data.Functor                   ( ($>) )
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                 hiding ( FilePath )
import qualified Prelude                       as P
import           Turtle

class CanTestPath m where
  pathExists :: FilePath -> m Bool

instance CanTestPath App where
    pathExists = testpath

class CanGetResponse m where
  getResponse :: Text -> m Text

instance CanGetResponse App where
    getResponse prompt = do
        report_ prompt
        rawResponse <- readline
        pure . lineToText . fromMaybe "" $ rawResponse

type Archiver m = ArchiveTarget -> m (Either Text ())
type ArchiveTargetMkr m = Album -> m (Either Text ArchiveTarget)

class MonadError Text m => CanArchive m where
  mkTarget :: ArchiveTargetMkr m
  doArchive :: Archiver m

instance CanArchive App where
    mkTarget album = do
        opts <- asks $ archiveOptions . config
        getTargetMkr opts album
    doArchive album = do
        opts <- asks $ archiveOptions . config
        archiverFromOptions opts album

data ArchiveTarget = ArchiveTarget
    { source :: Text
    , dest   :: Text
    }

data ArchiveFailure = Skipped | Error Text

archiveM
    :: forall m
     . ( MonadError Text m
       , CanArchive m
       , Monad m
       , Logs m
       , CanTestPath m
       , CanGetResponse m
       )
    => [Album]
    -> m [Album]
archiveM albums = do
    targetResults   <- mapMToSnd mkTarget albums
    targets         <- handleFailedTargets targetResults
    overwriteChecks <- P.mapM
        (\(alb, tgt) -> (, alb, tgt) <$> (checkOverwrite . dest $ tgt))
        targets
    let albumAndTargets = mapSkips <$> overwriteChecks
    -- TODO: Does this halt on first error?
    forM albumAndTargets $ \(album, tgt) -> do
        aoeu <- maybe (pure $ Right ()) doArchive tgt
        liftEither $ aoeu $> album
  where
    mapSkips :: (Bool, Album, ArchiveTarget) -> (Album, Maybe ArchiveTarget)
    mapSkips (True , alb, tgt) = (alb, Just tgt)
    mapSkips (False, alb, tgt) = (alb, Nothing)

handleFailedTargets
    :: Logs m
    => [(Album, Either Text ArchiveTarget)]
    -> m [(Album, ArchiveTarget)]
handleFailedTargets targets = do
    let (fails, targets') = partitionEithers
            $ fmap (\(alb, eith) -> bimap (alb, ) (alb, ) eith) targets
    report_ $ failMsgs fails
    pure targets'
  where
    failMsg (album, msg) =
        T.unwords [T.pack . encodeString $ relativePath album, msg]
    failMsgs fails =
        T.unlines
            $ "Could not construct destination for the following:"
            : fmap failMsg fails

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

getTargetMkr :: ArchiveOptions -> ArchiveTargetMkr App
getTargetMkr NoArchive             _     = pure $ Left "Nothing to do"
getTargetMkr (MoveArchive archDir) album = pure $ mkMoveTarget archDir album
getTargetMkr (ZipArchive  archDir) album = pure $ mkZipTarget archDir album

archiverFromOptions :: ArchiveOptions -> Archiver App
archiverFromOptions NoArchive                = const . pure . pure $ ()
archiverFromOptions (ZipArchive  _         ) = archiveZip
archiverFromOptions (MoveArchive archiveDir) = archiveMove

archiveZip :: MonadIO m => Archiver m
archiveZip target =
    biMap T.unwords (const ()) <$> reduce collectEithers (zap target)

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

archiveMove :: MonadIO m => Archiver m
archiveMove target@ArchiveTarget { dest } = do
    mktree $ fromText dest
    liftIO . single $ rsync target

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

checkOverwrite :: (Logs m, CanTestPath m, CanGetResponse m) => Text -> m Bool
checkOverwrite dest = do
    doesExist <- pathExists $ fromText dest
    if not doesExist then pure True else parseResponse dest

parseResponse :: (Logs m, CanGetResponse m) => Text -> m Bool
parseResponse dest = do
    response <- getResponse
        $ T.concat ["Files exist at: '", dest, "' Overwrite? [Y/n] "]
    case response of
        "y" -> pure True
        "Y" -> pure True
        ""  -> pure True
        "n" -> pure False
        "N" -> pure False
        _   -> do
            report "Unrecognized response."
            parseResponse dest
