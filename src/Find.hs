{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Find where

import           Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.HashMap                  as HM
import qualified Data.Text                     as T
import           Filesystem.Path
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

class CanFindAlbums m where
  findAlbums :: m [Album]

instance CanFindAlbums App where
  findAlbums = do
    flacs    <- findFlacs <$> asks (unMusicDir . musicDir . config)
    albumMap <- reduce albumMapFold flacs
    let albums = P.map mkAlbum $ toList albumMap
    pure albums

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
