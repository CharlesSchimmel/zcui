{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zcui.Archive
    ( explainArchiving
    , archiveM
    , Archives(..)
    ) where

import           Zcui.Archive.Class
import           Zcui.Archive.Types
import           Zcui.Class
import           Zcui.Types
import           Zcui.Util

import           Control.Monad                  ( forM )
import           Control.Monad.Except           ( MonadError(..)
                                                , liftEither
                                                )
import           Control.Monad.Reader           ( asks )
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Either                    ( partitionEithers )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import           Prelude                 hiding ( FilePath )
import qualified Prelude                       as P
import           Turtle

explainArchiving :: ArchiveOptions -> Text
explainArchiving NoArchive = "Skipping archiving"
explainArchiving (MoveArchive (ArchiveDir path)) =
    T.unwords ["Moving albums to", _toText path]
explainArchiving (ZipArchive (ArchiveDir path)) =
    T.unwords ["Zipping albums to", _toText path]

archiveM
    :: forall m
     . (Monad m, Archives m, Logs m, MonadError Text m)
    => [Album]
    -> m [Album]
archiveM albums = do
    targetResults   <- mapMToSnd mkTarget albums
    targets         <- reportAndIgnoreFailedTargets targetResults
    overwriteChecks <- P.mapM (uncurry checkOverwrite') targets
    -- TODO: Does this halt on first error?
    forM overwriteChecks $ \(album, target) -> do
        liftEither =<< maybe (skip album) (archive' album) target
        pure album
  where
    archive' album target = do
        report_ $ T.concat [artistAlbum album, "..."]
        archive target <* report "Done"
    skip album = do
        report . T.unwords $ ["Skipping", artistAlbum album]
        pure $ Right ()
    checkOverwrite' :: Album -> ArchiveTarget -> m (Album, Maybe ArchiveTarget)
    checkOverwrite' album target@ArchiveTarget { dest } = do
        res <- checkOverwrite dest
        pure $ if res then (album, Just target) else (album, Nothing)

reportAndIgnoreFailedTargets
    :: (Monad m, Logs m)
    => [(Album, Either Text ArchiveTarget)]
    -> m [(Album, ArchiveTarget)]
reportAndIgnoreFailedTargets targets = do
    let (fails, okTargets) = partitionEithers
            $ fmap (\(alb, eith) -> bimap (alb, ) (alb, ) eith) targets
    reportFails fails
    pure okTargets
  where
    reportFails fails =
        if P.null fails then pure () else report_ $ failMsgs fails
    failMsgs fails =
        T.unlines
            $ "Could not construct destination for the following:"
            : fmap failMsg fails
    failMsg (album, msg) =
        T.unwords [T.pack . encodeString $ relativePath album, msg]
