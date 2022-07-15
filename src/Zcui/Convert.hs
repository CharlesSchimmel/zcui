{-# LANGUAGE ScopedTypeVariables #-}

module Zcui.Convert
    ( convertM
    , Converts(..)
    , FileProjection(..)
    , ConvertedSong(..)
    ) where

import           Zcui.Class                     ( Logs(..) )
import           Zcui.Convert.Class
import           Zcui.Convert.Types
import           Zcui.Files
import           Zcui.Types

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader           ( asks )
import           Data.Either
import           Data.Text                     as T
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle                  hiding ( (<&>) )

convertM
    :: forall m
     . (Logs m, Converts m, MonadError Text m)
    => [Song]
    -> m [ConvertedSong]
convertM songs = do
    songsToConvert <- forM songs $ \song -> do
        target <- liftEither $ mkTarget song
        return (song, target)
    forM songsToConvert $ \(song, conversion) -> do
        report_ . T.concat $ [songFileName song, "..."]
        liftEither =<< convertSong conversion
        return . ConvertedSong song . Song . fromText $ dest conversion

instance Converts App where
    convertSong song = do
        isDryRun <- asks $ dryRun . config
        if isDryRun
            then pure $ Right ()
            else do
                bitrateToUse <- asks $ bitrate . conversionOptions . config
                single (convertToOgg bitrateToUse song)

mkTarget :: Song -> Either Text FileProjection
mkTarget song@(Song path) = FileProjection <$> toText path <*> toText oggPath
    where oggPath = replaceExtension path "ogg"

convertToOgg :: Bitrate -> FileProjection -> Shell (Either Text ())
convertToOgg bitrate FileProjection { source, dest } = do
    result <- proc
        "ffmpeg"
        [ "-loglevel"
        , "error"
        , "-hide_banner"
        , "-y"
        , "-i"
        , source
        , "-acodec"
        , "libopus"
        , "-b:a"
        , textBitrate bitrate
        , "-vbr"
        , "on"
        , "-vn"
        , "-compression_level"
        , "10"
        , dest
        ]
        (pure mempty)
    case result of
        ExitSuccess   -> pure . Right $ ()
        ExitFailure _ -> pure . Left $ "Failed converting"

textBitrate :: Bitrate -> Text
textBitrate (Bitrate num) = T.pack . (++ "k") . show $ num
