{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Zcui.Convert
    ( convertM
    ) where

import           Zcui.Types

import           Zcui.Class                     ( Logs(..) )

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

data Conversion = Conversion
    { inputPath  :: Text
    , outputPath :: Text
    }

class Converts m where
  convertSong :: Conversion -> m (Either Text ())

instance Converts App where
    convertSong song = do
        isDryRun <- asks $ dryRun . config
        if isDryRun
            then pure $ Right ()
            else do
                bitrateToUse <- asks $ bitrate . conversionOptions . config
                single (convertToOgg bitrateToUse song)

convertM
    :: forall m
     . (Logs m, Converts m, MonadError Text m)
    => [Song]
    -> m [ConvertedSong]
convertM songs = do
    songsToConvert <- liftEither $ mapM mkConversion songs
    mapM_ convertSong' songsToConvert
    pure $ P.map mkConvertedSong songs
  where
    convertSong' :: (Song, Conversion) -> m ()
    convertSong' (song, conversion) = do
        report_ . T.concat $ [songFileName song, "..."]
        liftEither =<< convertSong conversion
    mkConvertedSong s@(Song path) =
        ConvertedSong s . Song $ replaceExtension path ".ogg"

mkConversion :: Song -> Either Text (Song, Conversion)
mkConversion song@(Song path) =
    (song, ) <$> (Conversion <$> toText path <*> toText oggPath)
    where oggPath = replaceExtension path "ogg"

convertToOgg :: Bitrate -> Conversion -> Shell (Either Text ())
convertToOgg bitrate (Conversion origPath oggPath) = do
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
