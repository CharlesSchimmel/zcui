module Zcui where

import           Zcui.Archive
import           Zcui.Class
import           Zcui.Convert
import           Zcui.Delete
import           Zcui.Find
import           Zcui.Import
import           Zcui.Types
import           Zcui.Util

import           Control.Monad.Reader           ( asks )
import           Control.Monad.Trans.Maybe      ( runMaybeT )
import           Data.Functor
import           Data.Text                     as T
import           Prelude                       as P
                                         hiding ( FilePath )

zcui :: App ()
zcui = do
    albums <- findAlbums
    if P.null albums
        then report "No albums found."
        else do
            report_ . T.unlines $ "Found albums:" : P.map artistAlbum albums

            void . runMaybeT $ do
                archived <- asMaybeT_ albums $ \album -> do
                    report . explainArchiving =<< asks (archiveOptions . config)
                    archiveM album

                converted <- asMaybeT_ archived $ \album -> do
                    report "Converting"
                    convertM . (>>= songs) $ album

                _ <- asMaybeT_ converted $ deleteSongsM . P.map originalSong

                _ <- asMaybeT_ archived $ const updateM

                _ <- asMaybeT_ archived importM

                return ()
    report "All done :)"
