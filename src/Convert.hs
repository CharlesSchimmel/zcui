module Convert
  ( convertM
  )
where

import           Types

import           Control.Monad.Reader           ( MonadReader )
import           Control.Monad
import           Control.Monad.Except
import           Data.Either
import           Data.Functor
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle                  hiding ( (<&>) )

class CanConvert m where
  convertSong :: Song -> m ConvertedSong

instance CanConvert App where
  convertSong = goConvert

convertM :: (CanConvert m, Logs m) => [Song] -> m [ConvertedSong]
convertM songs = do
  report "Converting..."
  mapM convertSong songs

goConvert :: (MonadIO m, Logs m, MonadError Text m) => Song -> m ConvertedSong
goConvert song@(Song songPath) = do
  report fileName
  result <- single . doConvert $ song
  either throwError pure result
 where
  fileName =
    fromRight "(oops, could not toText song path)" (toText $ filename songPath)

doConvert :: Song -> Shell (Either Text ConvertedSong)
doConvert song@Song {..} =
  either (pure . Left) id $ convert' <$> textSongPath <*> textOggPath
 where
  oggPath       = replaceExtension songPath "ogg"
  textSongPath  = toText songPath
  textOggPath   = toText oggPath
  convertedSong = ConvertedSong song (Song oggPath)
  convert' orig ogg = convertToOgg orig ogg <&> ($> convertedSong)

convertToOgg :: Text -> Text -> Shell (Either Text ())
convertToOgg origPath oggPath = do
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
    , "128k"
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
