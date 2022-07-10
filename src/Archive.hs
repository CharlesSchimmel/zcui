{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Archive
    ( explainArchiving
    , archiveM
    ) where

import           Archive.Class
import           Archive.Types
import           Class
import           Types
import           Util

import qualified Control.Foldl                 as Fold
import           Control.Monad
import           Control.Monad.Except           ( MonadError(..)
                                                , liftEither
                                                )
import           Control.Monad.Reader           ( asks )
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Either
import           Data.Functor                   ( ($>) )
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Prelude                 hiding ( FilePath )
import qualified Prelude                       as P
import           Turtle

instance Archives App where
    mkTarget album = do
        opts <- asks $ archiveOptions . config
        getTargetMkr opts album
    archive album = do
        opts <- asks $ archiveOptions . config
        archiverFromOptions opts album

archiveM
    :: forall m
     . ( MonadError Text m
       , Archives m
       , Monad m
       , Logs m
       , TestsPath m
       , Prompts m
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
        aoeu <- maybe (pure $ Right ()) archive tgt
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

explainArchiving :: ArchiveOptions -> Text
explainArchiving NoArchive = "Skipping archiving"
explainArchiving (MoveArchive (ArchiveDir path)) =
    T.unwords ["Moving albums to", _toText path]
explainArchiving (ZipArchive (ArchiveDir path)) =
    T.unwords ["Zipping albums to", _toText path]

getTargetMkr :: ArchiveOptions -> ArchiveTargetMkr App
getTargetMkr NoArchive             _     = pure $ Left "Nothing to do"
getTargetMkr (MoveArchive archDir) album = pure $ mkMoveTarget archDir album
getTargetMkr (ZipArchive  archDir) album = pure $ mkZipTarget archDir album

mkZipTarget :: ArchiveDir -> Album -> Either Text ArchiveTarget
mkZipTarget (ArchiveDir archDir) album@Album { absolutePath = albumDir } = do
    let zipName    = fromText $ T.concat [artistAlbum album, ".7z"]
        outputFile = archDir </> zipName
    source <- toText albumDir
    dest   <- toText outputFile
    pure $ ArchiveTarget source dest

mkMoveTarget :: ArchiveDir -> Album -> Either Text ArchiveTarget
mkMoveTarget (ArchiveDir archDir) album@Album { absolutePath, relativePath } =
    do
        source <- toText absolutePath
        dest   <- toText $ archDir </> relativePath
        pure $ ArchiveTarget source dest

archiverFromOptions :: ArchiveOptions -> Archiver App
archiverFromOptions NoArchive                = const . pure . pure $ ()
archiverFromOptions (ZipArchive  _         ) = archiveZip
archiverFromOptions (MoveArchive archiveDir) = archiveMove

archiveZip :: MonadIO m => Archiver m
archiveZip target =
    biMap T.unwords (const ()) <$> reduce collectEithers (zap target)

zap :: ArchiveTarget -> Shell (Either Text ())
zap ArchiveTarget { source = folderToZip, dest = zipDest } = do
    result <- inprocWithErr "7z" ["a", zipDest, folderToZip] (pure mempty)
    return $ either (Left . linesToText . pure) (const $ Right ()) result

archiveMove :: MonadIO m => Archiver m
archiveMove target@ArchiveTarget { dest } = do
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
                $ ["Rsync failed to sync album from", source, " to ", dest]

checkOverwrite :: (Logs m, TestsPath m, Prompts m) => Text -> m Bool
checkOverwrite dest = do
    doesExist <- pathExists $ fromText dest
    if not doesExist then pure True else parseResponse dest

parseResponse :: (Logs m, Prompts m) => Text -> m Bool
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
