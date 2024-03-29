module Zcui.Options
    ( doParseArgs
    , Arguments(..)
    , getConfig
    ) where

import           Zcui.Types

import           Data.Text                     as T
import           Prelude                 hiding ( FilePath )
import           Turtle

getConfig :: IO (Either Text Config)
getConfig = doParseArgs >>= verifyOptions

data Arguments = Arguments
    { argMusicDir          :: MusicDir
    , argArchiveOptions    :: ArchiveOptions
    , argConversionOptions :: ConversionOptions
    , argDryRun            :: Bool
    }

doParseArgs :: IO Arguments
doParseArgs = options "Zip Convert Update Import" argParse

argParse :: Parser Arguments
argParse =
    Arguments
        <$> musicDirParse
        <*> archiveOpts
        <*> conversionsOpts
        <*> dryRunParse

dryRunParse :: Parser Bool
dryRunParse = switch
    "dry-run"
    'd'
    "Do not perform any destructive actions and fake out results"

musicDirParse :: Parser MusicDir
musicDirParse =
    MusicDir <$> optPath "library-dir" 'l' "Music library directory to search"

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
        <$> optInt "bitrate" 'b' "Average output bitrate. Default 96"
        <|> pure defaultBitrate

defaultBitrate = Bitrate 96

verifyOptions :: Arguments -> IO (Either Text Config)
verifyOptions Arguments { argMusicDir, argArchiveOptions, argDryRun, argConversionOptions }
    = do
        music   <- testMusicDir argMusicDir
        archive <- testArchive argArchiveOptions
        let convOpts = testConversionOptions argConversionOptions
        pure $ (Config <$> music <*> archive <*> convOpts <*> pure argDryRun)

testConversionOptions copts@(ConversionOptions (Bitrate bitrate)) =
    if bitrate > 32 && bitrate < 512
        then pure copts
        else Left "Bitrate must be between 32-512"

testMusicDir :: MusicDir -> IO (Either Text MusicDir)
testMusicDir musicDir@(MusicDir path) = fmap (const musicDir) <$> testdir' path

testArchive :: ArchiveOptions -> IO (Either Text ArchiveOptions)
testArchive NoArchive = pure . pure $ NoArchive
testArchive opts@(MoveArchive (ArchiveDir archDir)) =
    fmap (const opts) <$> testdir' archDir
testArchive opts@(ZipArchive (ArchiveDir archDir)) =
    fmap (const opts) <$> testdir' archDir

testdir' :: FilePath -> IO (Either Text FilePath)
testdir' path = do
    pathExists <- testdir path
    if pathExists
        then pure . Right $ path
        else
            pure
            . Left
            . T.append "Could not find path: "
            . T.pack
            . show
            $ path
