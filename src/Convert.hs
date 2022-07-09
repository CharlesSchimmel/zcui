module Convert
    ( convertM
    ) where

import           Types

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader           ( asks )
import           Data.Either
import           Data.Functor
import           Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle                  hiding ( (<&>) )

data SongToConvert = SongToConvert
    { inputPath  :: Text
    , outputPath :: Text
    }

class CanConvert m where
  convertSong :: SongToConvert -> m ()

instance CanConvert App where
    convertSong song = do
        isDryRun <- asks $ dryRun . config
        if isDryRun
            then pure ()
            else do
                bitrateToUse <- asks $ bitrate . conversionOptions . config
                liftEither =<< single (convertToOgg bitrateToUse song)

convertM :: (CanConvert m, MonadError Text m) => [Song] -> m [ConvertedSong]
convertM songs = do
    let convertedSongs = P.map mkConvertedSong songs
    songsToConvert <- liftEither $ mapM massageSong songs
    mapM convertSong songsToConvert $> convertedSongs
  where
    mkConvertedSong s@(Song path) =
        ConvertedSong s . Song $ replaceExtension path ".ogg"

massageSong :: Song -> Either Text SongToConvert
massageSong (Song path) = SongToConvert <$> toText path <*> toText oggPath
    where oggPath = replaceExtension path "ogg"

convertToOgg :: Bitrate -> SongToConvert -> Shell (Either Text ())
convertToOgg bitrate (SongToConvert origPath oggPath) = do
    result <- proc
        "ffmpeg"
        [ "-loglevel"
        , "error"
        , "-hide_banner"
        , "-y"
        , "-i"
        , origPath
        , "-acodec"
        , "libopus"
        , "-b:a"
        , textBitrate bitrate
        , "-vbr"
        , "on"
        , "-vn"
        , "-compression_level"
        , "10"
        , oggPath
        ]
        (pure mempty)
    case result of
        ExitSuccess   -> pure . Right $ ()
        ExitFailure _ -> pure . Left $ "Failed converting"

textBitrate :: Bitrate -> Text
textBitrate (Bitrate num) = T.pack . (++ "k") . show $ num
