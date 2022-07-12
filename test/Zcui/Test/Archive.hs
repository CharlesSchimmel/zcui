{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Zcui.Test.Archive where

import           Test.Hspec

import           Zcui.Test.Data                 ( albums )

import           Zcui.Archive                   ( archiveM )
import           Zcui.Archive.Class             ( Archives(..) )
import           Zcui.Archive.Types             ( ArchiveTarget(ArchiveTarget) )
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

data MockEnv = MockEnv
    { overwriteResponse :: Text
    , pathExistResult   :: Bool
    , archiveResult     :: Either Text ()
    , targetResult      :: Either Text ArchiveTarget
    }

newtype MockM a = MockM
    { runEnv :: ReaderT MockEnv (ExceptT Text Identity) a
    } deriving (Monad, Functor, Applicative, MonadError Text, MonadReader MockEnv)

instance Archives MockM where
    mkTarget _ = do
        asks targetResult
    archive target = do
        asks archiveResult

instance Logs MockM where
    report_ msg = pure ()

instance Prompts MockM where
    getResponse promptMsg = do
        response <- asks overwriteResponse
        report $ T.unlines [promptMsg, response]
        pure response

instance TestsPath MockM where
    pathExists _ = asks pathExistResult

runMock env = runIdentity . runExceptT . flip runReaderT env . runEnv
runArchiveMock env = runMock env $ archiveM albums

defTarget = ArchiveTarget "source" "dest"
happyEnv = MockEnv "y" False (Right ()) (Right defTarget)

archiveTests :: Spec
archiveTests = describe "Archive" $ do
    happyPath
    fileConflicts
    archiveFail

happyPath =
    describe "When there are no file conflicts and archiving succeeds" $ do
        let result = runMock happyEnv $ archiveM albums
        it "then all albums returned" $ result `shouldBe` Right albums

fileConflicts = describe "When destination files exist" $ do
    let env = happyEnv { pathExistResult = True }

    describe "and the user does not overwrite" $ do
        let env'   = env { overwriteResponse = "n" }
        let result = runMock env' $ archiveM albums
        it "then all albums returned" $ result `shouldBe` Right albums

    describe "and the user does overwrite" $ do
        let env'   = env { overwriteResponse = "y" }
        let result = runMock env' $ archiveM albums
        it "then all albums returned" $ result `shouldBe` Right albums

archiveFail = describe "When archive operation fails" $ do
    it "then exit immediately" $ do
        isLeft (runArchiveMock happyEnv { archiveResult = Left "Oh no" })
            `shouldBe` True

