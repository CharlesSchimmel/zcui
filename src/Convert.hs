{-# LANGUAGE OverloadedStrings #-}

module Convert where

import Types

import qualified Control.Foldl as Fold
import Control.Monad
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

trace_ i = trace (show i) i

convertM :: [Song] -> App [ConvertedSong]
convertM songs = do
  result <- reduce multiConvertFold $ converts songs
  either throwError pure result

multiConvertFold ::
     Fold (Either Text ConvertedSong) (Either Text [ConvertedSong])
multiConvertFold = Fold (flip $ liftM2 (:)) (Right []) id

converts :: [Song] -> Shell (Either Text ConvertedSong)
converts songs = cat $ P.map doConvert songs

doConvert :: Song -> Shell (Either Text ConvertedSong)
doConvert song@(Song songPath) = (convertedSong <$) <$> convertResult
  where
    oggPath = replaceExtension songPath "ogg"
    textSongPath = toText songPath
    textOggPath = toText oggPath
    convertResult = swap $ convertToOgg <$> textSongPath <*> textOggPath
    convertedSong = ConvertedSong song (Song oggPath)

bigSwap :: Either Text (Shell (Either Line Line)) -> Shell (Either Text Text)
bigSwap = either (pure . Left) subSwap
  where
    subSwap b = either (pure . lineToText) (pure . lineToText) <$> b

convertToOgg :: Text -> Text -> Shell (Either Text ())
convertToOgg origPath oggPath = do
  result <-
    proc
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
    ExitSuccess -> pure . Right $ ()
    ExitFailure _ -> pure . Left $ "Failed converting"

deleteSongsM :: [Song] -> App ()
deleteSongsM = sh . cat . P.map deleteSong

deleteSong :: Song -> Shell ()
deleteSong (Song songPath) = rm songPath
