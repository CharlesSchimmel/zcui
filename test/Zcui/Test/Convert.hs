{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Zcui.Test.Convert where

import           Test.Hspec

import           Zcui.Test.Class
import           Zcui.Test.Data                 ( albums )

import           Zcui.Convert                   ( ConvertedSong(..)
                                                , Converts(..)
                                                , convertM
                                                )
import           Zcui.Types                     ( Album(..)
                                                , Song(..)
                                                )

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Either                    ( fromLeft
                                                , fromRight
                                                , isLeft
                                                , isRight
                                                )
import           Data.Text                     as T
import           Debug.Trace                    ( trace )
import           Filesystem.Path.CurrentOS
import           Prelude                       as P
                                         hiding ( FilePath )

newtype ConvertEnv = ConvertEnv
    { convertResult :: Either Text ()
    }

type ConvertMock = MockM ConvertEnv
instance Converts ConvertMock where
    convertSong _ = do
        asks convertResult

mockSong = Song $ fromText "song.flac"
mockSongs = [mockSong]
runConvertMock env = runMock env $ convertM mockSongs

happyEnv = ConvertEnv $ pure ()

convertTests :: Spec
convertTests = describe "Convert" $ do
    happyPath
    conversionFails

happyPath = describe "When conversion succeeds" $ do
    let result = runConvertMock happyEnv
    it "then succeed" $ do
        isRight result
    it "then return converted song" $ do
        let converted = ConvertedSong mockSong . Song $ fromText "song.ogg"
        let assert    = either (const False) ((converted ==) . P.head) result
        assert `shouldBe` True

conversionFails = describe "When conversion fails" $ do
    let result = runConvertMock . ConvertEnv $ Left "Oh no"
    it "then halt entirely" $ do
        isLeft result
