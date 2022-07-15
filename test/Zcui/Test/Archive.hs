{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Zcui.Test.Archive where

import           Test.Hspec

import           Zcui.Test.Class
import           Zcui.Test.Data                 ( albums )

import           Zcui.Archive                   ( archiveM )
import           Zcui.Archive.Class             ( Archives(..) )
import           Zcui.Archive.Types             ( FileProjection(..) )
import           Zcui.Class                     ( Logs(..)
                                                , Prompts(..)
                                                , TestsPath(..)
                                                )
import           Zcui.Types                     ( Album(..) )

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Either                    ( isLeft
                                                , isRight
                                                )
import           Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Prelude                       as P
                                         hiding ( FilePath )

data ArchiveEnv = ArchiveEnv
    { overwriteResponse :: Text
    , pathExistResult   :: Bool
    , archiveResult     :: Either Text ()
    , targetResult      :: Either Text FileProjection
    }

type ArchiveMock = MockM ArchiveEnv

instance Archives ArchiveMock where
    mkTarget _ = do
        asks targetResult
    archive target = do
        asks archiveResult

instance Prompts ArchiveMock where
    getResponse promptMsg = do
        asks overwriteResponse

instance TestsPath ArchiveMock where
    pathExists _ = asks pathExistResult

runArchiveMock env = runMock env $ archiveM albums

defTarget = FileProjection "source" "dest"
happyEnv = ArchiveEnv "y" False (Right ()) (Right defTarget)

archiveTests :: Spec
archiveTests = describe "Archive" $ do
    happyPath
    fileConflicts
    archiveFail
    targetFail

happyPath =
    describe "When there are no file conflicts and archiving succeeds" $ do
        let result = runArchiveMock happyEnv
        it "then all albums returned" $ result `shouldBe` Right albums

fileConflicts = describe "When destination files exist" $ do
    let env = happyEnv { pathExistResult = True }

    describe "and the user does not overwrite" $ do
        let env' = env { overwriteResponse = "n" }
        it "then all albums returned" $ do
            let result = runArchiveMock env'
            result `shouldBe` Right albums

    describe "and the user does overwrite" $ do
        let env' = env { overwriteResponse = "y" }
        it "then all albums returned" $ do
            let result = runArchiveMock env'
            result `shouldBe` Right albums

archiveFail = describe "When archive operation fails" $ do
    it "then exit immediately" $ do
        let result = runArchiveMock happyEnv { archiveResult = Left "Oh no" }
        isLeft result

targetFail = describe "When making a target fails" $ do
    let failedTargetEnv = happyEnv { targetResult = Left "Oh no" }

    it "then keep going" $ do
        let result = runArchiveMock failedTargetEnv
        isRight result

    it "then don't continue with failed albums" $ do
        let oneAlbum = P.take 1 albums
        let result = P.length <$> runMock failedTargetEnv (archiveM oneAlbum)
        result `shouldBe` Right 0


