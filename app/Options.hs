{-# LANGUAGE OverloadedStrings #-}

module Options where

import Lib
import Types

import Turtle

optionsParse = Options <$> musicDirParse <*> archiveParse

musicDirParse = MusicDir <$> optPath "musicDir" 'm' "Music directory to search"

archiveParse = parseMove <|> parseZip <|> pure NoArchive

parseArchiveDir = optPath "archiveDir" 'a' "Archive directory"

doMove = switch "move" 'v' "Archive and move"

doZip = switch "zip" 'z' "Archive and zip"

parseMove = MoveArchive . ArchiveDir <$> (doMove *> parseArchiveDir)

parseZip = ZipArchive . ArchiveDir <$> (doZip *> parseArchiveDir)
