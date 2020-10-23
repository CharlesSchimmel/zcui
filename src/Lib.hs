module Lib where

import           Archive
import           Convert
import           Import
import           Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.HashMap                  as HM
import qualified Data.Text                     as T
import           Filesystem.Path
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

findFlacs :: FilePath -> Shell FilePath
findFlacs = find (suffix ".flac")

type AlbumPath = FilePath

mkAlbum :: (AlbumPath, [Song]) -> Album
mkAlbum (albumPath, songs) = Album
    { baseDir    = albumPath
    , albumName  = _toText . dirname . directory $ albumPath
    , artistName = _toText . dirname . parent . directory $ albumPath
    , songs      = songs
    }

albumMapFold :: Fold FilePath (Map AlbumPath [Song])
albumMapFold = Fold foldy HM.empty id
  where
    foldy :: Map AlbumPath [Song] -> FilePath -> Map AlbumPath [Song]
    foldy accumulatedMap flacPath =
        insertWith (++) (directory flacPath) [Song flacPath] accumulatedMap

doFindAlbums :: App [Album]
doFindAlbums = do
    flacs    <- findFlacs <$> asks (unMusicDir . musicDir)
    albumMap <- reduce albumMapFold flacs
    let albums = P.map mkAlbum $ toList albumMap
    if P.null albums then throwError "No albums found." else pure albums

findAlbumsM :: App [Album]
findAlbumsM = do
    albums <- doFindAlbums
    report . T.unlines $ "Found albums:" : P.map artistAlbum albums
    return albums

zcuiM :: App ()
zcuiM = do
    albums         <- findAlbumsM
    archived       <- archivesM albums
    convertedSongs <- convertM (archived >>= songs)
    _              <- deleteSongsM $ P.map originalSong convertedSongs
    _              <- updateM
    _              <- importM archived
    report "All done :)"
