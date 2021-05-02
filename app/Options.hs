{-# LANGUAGE OverloadedStrings #-}

module Options where

import           Types

import           Turtle
import           Data.Functor
import           Control.Applicative
import           Control.Monad

optionsParse = Options <$> musicDirParse <*> archType

musicDirParse = MusicDir <$> optPath "musicDir" 'm' "Music directory to search"

archiveParse = parseMove <|> parseZip <|> pure NoArchive

parseArchiveDir = optPath "archiveDir" 'a' "Archive directory"

doMove = switch "move" 'v' "Move files to archive"

doZip = switch "zip" 'z' "Zip files to archive"

parseMove = MoveArchive . ArchiveDir <$> (doMove *> parseArchiveDir)

parseZip = ZipArchive . ArchiveDir <$> (doZip *> parseArchiveDir)

archDir = ArchiveDir <$> optPath "archiveDir" 'a' "Archive directory"

moveArch :: Parser (ArchiveDir -> ArchiveOptions)
moveArch = doMove $> MoveArchive

zipArch :: Parser (ArchiveDir -> ArchiveOptions)
zipArch = doZip $> ZipArchive

noArch :: Parser ArchiveOptions
noArch = pure NoArchive

archType = noArch <|> (archDir <**> moveOrZip)
  where moveOrZip = zipArch <|> moveArch
