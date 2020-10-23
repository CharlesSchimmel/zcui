module Convert
  ( convertM
  , deleteSongsM
  )
where

import           Types

import           Control.Monad
import           Control.Monad.Except
import           Data.Either
import           Data.Functor
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle                  hiding ( (<&>) )

convertM :: [Song] -> App [ConvertedSong]
convertM songs = do
  report "Converting..."
  mapM goConvert songs

goConvert :: Song -> App ConvertedSong
goConvert song@(Song songPath) = do
  liftIO . putStrLn . T.unpack $ fileName
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

deleteSongsM :: [Song] -> App ()
deleteSongsM songs = do
  report "Deleting"
  void $ mapM deleteSong songs

deleteSong :: Song -> App ()
deleteSong (Song songPath) = do
  liftIO . putStrLn . T.unpack $ fileNameText
  sh $ rm songPath
 where
  fileNameText =
    fromRight "(oops, could not toText song path)" (toText $ filename songPath)
