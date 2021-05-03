module Options where

import           Types

import           Turtle
import           Data.Functor
import           Control.Applicative


data Arguments = Arguments
  { argMusicDir       :: MusicDir
  , argArchiveOptions :: ArchiveOptions
  }

doParseArgs :: IO Arguments
doParseArgs = options "Zip Convert Update Import" argParse

argParse = Arguments <$> musicDirParse <*> archType

musicDirParse = MusicDir <$> optPath "musicDir" 'm' "Music directory to search"

doMove = switch "move" 'v' "Move files to archive"

doZip = switch "zip" 'z' "Zip files to archive"

archDir = ArchiveDir <$> optPath "archiveDir" 'a' "Archive directory"

moveArch :: Parser (ArchiveDir -> ArchiveOptions)
moveArch = doMove $> MoveArchive

zipArch :: Parser (ArchiveDir -> ArchiveOptions)
zipArch = doZip $> ZipArchive

noArch :: Parser ArchiveOptions
noArch = pure NoArchive

archType = noArch <|> (archDir <**> moveOrZip)
  where moveOrZip = zipArch <|> moveArch
