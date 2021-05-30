{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Find where

import           Types
import           Util

import           Control.Monad.Reader
import           Data.HashMap                  as HM
import           Data.Maybe                     ( fromMaybe )
import           Filesystem.Path
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

class CanFindAlbums m where
  findAlbums :: m [Album]

instance CanFindAlbums App where
  findAlbums = do
    musicDir <- asks $ musicDir . config
    albumMap <- reduce albumMapFold $ findFlacs $ unMusicDir musicDir
    let albums = P.map (mkAlbum musicDir) $ toList albumMap
    pure albums

findFlacs :: FilePath -> Shell FilePath
findFlacs = find (suffix ".flac")

type AlbumPath = FilePath

mkAlbum :: MusicDir -> (AlbumPath, [Song]) -> Album
mkAlbum (MusicDir rootDir) (albumPath, songs) = Album
  { absolutePath = albumPath
  , relativePath = relPath
  , albumName    = _toText . dirname . directory $ albumPath
  , artistName   = _toText . dirname . parent . directory $ albumPath
  , songs        = songs
  }
 where
  rootWithTrailingSlash = rootDir </> mempty
  relPath =
    fromMaybe "impossible" $ stripPrefix rootWithTrailingSlash albumPath

albumMapFold :: Fold FilePath (Map AlbumPath [Song])
albumMapFold = Fold foldy HM.empty id
 where
  foldy :: Map AlbumPath [Song] -> FilePath -> Map AlbumPath [Song]
  foldy accumulatedMap flacPath =
    insertWith (++) (directory flacPath) [Song flacPath] accumulatedMap
