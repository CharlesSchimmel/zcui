module Lib where

import           Archive
import           Convert
import           Delete
import           Import
import           Find
import           Types
import           Data.Text                     as T

import           Control.Monad.Except
import           Prelude                       as P
                                         hiding ( FilePath )

zcuiM :: App ()
zcuiM = do
    albums <- findAlbums
    if (P.null albums)
        then report "No albums found."
        else do
            report . T.unlines $ "Found albums:" : P.map artistAlbum albums

            report =<< getArchiveStatus
            archived <- archiveM albums

            report "Converting..."
            convertedSongs <- convertM (archived >>= songs)

            report "Deleting"
            _ <- deleteSongsM $ P.map originalSong convertedSongs

            report "Updating beets"
            _ <- updateM

            _ <- importM archived
            pure ()
    report "All done :)"
