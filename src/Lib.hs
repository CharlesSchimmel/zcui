{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Archive
import Convert
import Import
import Types

import Control.Monad.Except
import Control.Monad.Reader
import Data.Either
import Data.HashMap as HM
import Data.Hashable
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace
import Filesystem.Path
import Filesystem.Path.CurrentOS
import Prelude as P hiding (FilePath)
import Turtle

someFunc :: IO ()
someFunc = putStrLn "someFunc"

findFlacs :: FilePath -> Shell FilePath
findFlacs = find (suffix ".flac")

type AlbumPath = FilePath

mkAlbum :: (AlbumPath, [Song]) -> Album
mkAlbum (albumPath, songs) =
  Album
    { baseDir = albumPath
    , albumName = _toText . dirname . directory $ albumPath
    , artistName = _toText . dirname . parent . directory $ albumPath
    , songs = songs
    }

albumMapFold :: Fold FilePath (Map AlbumPath [Song])
albumMapFold = Fold foldy HM.empty id
  where
    foldy :: Map AlbumPath [Song] -> FilePath -> Map AlbumPath [Song]
    foldy accumulatedMap flacPath =
      insertWith (++) (directory flacPath) [Song flacPath] accumulatedMap

findAlbums :: MusicDir -> Shell [Album]
findAlbums (MusicDir musFolder) = do
  albumMap <- reduce albumMapFold $ findFlacs musFolder
  let albums = P.map mkAlbum $ toList albumMap
  return albums

findAlbumsM :: App [Album]
findAlbumsM = do
  flacs <- findFlacs <$> asks (unMusicDir . musicDir)
  albumMap <- reduce albumMapFold flacs
  let albums = P.map mkAlbum $ toList albumMap
  if P.null albums
    then throwError "No albums found."
    else pure albums

convertDeleteAlbum :: Album -> App Album
convertDeleteAlbum album = do
  report "Converting..."
  converted <- convertM $ songs album
  report "Deleting..."
  report $ T.unwords $ P.map (T.pack . show . songPath . originalSong) converted
  deleted <- deleteSongsM (P.map originalSong converted)
  pure album

archiveStatus :: ArchiveOptions -> Text
archiveStatus NoArchive = "Skipping archiving"
archiveStatus (MoveArchive (ArchiveDir path)) =
  T.unwords
    [ "Moving albums to"
    , fromRight "could not parse archive path" . toText $ path
    ]
archiveStatus (ZipArchive (ArchiveDir path)) =
  T.unwords
    [ "Zipping albums to"
    , fromRight "could not parse archive path" . toText $ path
    ]

zcuiM :: App ()
zcuiM = do
  musicDir_ <- asks musicDir
  albums <- findAlbumsM
  report . T.unwords $ "Found albums:" : P.map artistAlbum albums
  archOpt <- asks archiveOptions
  report . archiveStatus $ archOpt
  archived <- archivesM albums
  report "Converting..."
  convertedAndDeleted <- mapM convertDeleteAlbum archived
  report "Updating..."
  sh beetUpdate
  report "Importing..."
  imported <- importM convertedAndDeleted
  report "All done :)"

report :: Text -> App ()
report = liftIO . print
