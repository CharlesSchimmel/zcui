module Options
  ( doParseArgs
  , Arguments(..)
  )
where

import           Types

import           Turtle


data Arguments = Arguments
  { argMusicDir          :: MusicDir
  , argArchiveOptions    :: ArchiveOptions
  , argConversionOptions :: ConversionOptions
  }

doParseArgs :: IO Arguments
doParseArgs = options "Zip Convert Update Import" argParse

argParse = Arguments <$> musicDirParse <*> archiveOpts <*> conversionsOpts

musicDirParse =
  MusicDir <$> optPath "music-dir" 'm' "Music directory to search"

doMove = switch "move" 'v' "Move files to archive"

doZip = switch "zip" 'z' "Zip files to archive"

archDir = ArchiveDir <$> optPath "archive-dir" 'a' "Archive directory"

moveArch :: Parser ArchiveOptions
moveArch = MoveArchive . ArchiveDir <$> optPath
  "move-dir"
  'm'
  "Archive by moving to this directory"

zipArch :: Parser ArchiveOptions
zipArch = ZipArchive . ArchiveDir <$> optPath
  "zip-dir"
  'z'
  "Archive by zipping to this directory"

-- TODO: Switch to optparse-applicative to make providing an archive type required
noArch :: Parser ArchiveOptions
noArch = switch "no-archive" 'n' "Do not archive" <&> const NoArchive

archiveOpts :: Parser ArchiveOptions
archiveOpts = zipArch <|> moveArch

conversionsOpts :: Parser ConversionOptions
conversionsOpts = ConversionOptions <$> bitrateParse

bitrateParse =
  Bitrate
    <$> optInt "bitrate" 'b' "Average output bitrate. Default 128"
    <|> pure defaultBitrate

defaultBitrate = Bitrate 128
