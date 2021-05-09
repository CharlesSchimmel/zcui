{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Archive
import           Convert
import           Delete
import           Import
import           Find
import           Types
import           Data.Text                     as T

import           Data.Functor
import           Control.Monad.Trans.Maybe
import           Control.Monad.Except
import           Prelude                       as P
                                         hiding ( FilePath )

type MaybeApp a = MaybeT App a

zcuiM :: App ()
zcuiM = do
    albums <- findAlbums
    if (P.null albums)
        then report "No albums found."
        else do
            report . T.unlines $ "Found albums:" : P.map artistAlbum albums

            void . runMaybeT $ do
                archived <- asMaybeT_ albums $ \a -> do
                    report =<< getArchiveStatus
                    archiveM a

                converted <- asMaybeT_ archived $ \a -> do
                    report "Converting"
                    convertM . (>>= songs) $ a

                _ <- asMaybeT_ converted $ deleteSongsM . P.map originalSong

                _ <- asMaybeT_ archived $ const updateM

                _ <- asMaybeT_ archived $ importM

                return ()
    report "All done :)"

asMaybeT_ = flip asMaybeT

asMaybeT :: Monad m => ([a] -> m b) -> [a] -> MaybeT m b
asMaybeT fn as =
    if P.null as then (MaybeT $ pure Nothing) else MaybeT $ (Just <$> (fn as))
