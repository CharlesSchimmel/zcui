module Lib where

import           Archive
import           Convert
import           Delete
import           Import
import           Find
import           Types

import           Control.Monad.Except
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

zcuiM :: App ()
zcuiM = do
    albums         <- findAlbumsM
    archived       <- archiveM albums
    convertedSongs <- convertM (archived >>= songs)
    _              <- deleteSongsM $ P.map originalSong convertedSongs
    _              <- updateM
    _              <- importM archived
    report_ "All done :)"
