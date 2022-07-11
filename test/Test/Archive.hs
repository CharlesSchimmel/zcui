{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Test.Archive where

import           Test.Hspec

import           Zcui.Archive.Class             ( Archives(..) )
import           Zcui.Archive.Types             ( ArchiveTarget(ArchiveTarget) )
import           Zcui.Class                     ( Logs(..)
                                                , Prompts(..)
                                                , TestsPath(..)
                                                )

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Prelude                       as P
                                         hiding ( FilePath )
import           Zcui.Archive                   ( archiveM )
import           Zcui.Types                     ( Album(..) )

data MockEnv = MockEnv
    { overwriteResponse :: Text
    , pathExistResult   :: Bool
    , reportWith        :: Text -> IO ()
    }

newtype MockM a = MockM
    { runEnv :: ReaderT MockEnv (ExceptT Text IO) a
    } deriving (Monad, Functor, Applicative, MonadIO, MonadError Text, MonadReader MockEnv)

instance Archives MockM where
    mkTarget album = pure . Right $ ArchiveTarget "source" "dest"
    archive target = pure . pure $ ()

instance Logs MockM where
    report_ msg = do
        logr <- asks reportWith
        liftIO $ logr msg

instance Prompts MockM where
    getResponse promptMsg = do
        response <- asks overwriteResponse
        report $ T.unlines [promptMsg, response]
        pure response

instance TestsPath MockM where
    pathExists _ = asks pathExistResult

mkAlbum artist albumName = Album { relativePath = "/dev/null"
                                 , absolutePath = "null"
                                 , albumName    = albumName
                                 , artistName   = artist
                                 , songs        = []
                                 }
albums =
    [ mkAlbum "Esquivel"       "Space Age Bachelor Music"
    , mkAlbum "Alice Coltrane" "Journey in Satchidananda"
    , mkAlbum "TR/ST"          "The Destroyer - 1"
    ]


happyPath :: IO ()
happyPath = do
    let env = MockEnv "y" False T.putStr
    runExceptT . flip runReaderT env . runEnv $ do
        archiveM albums
    pure ()

whenPathExistsNoOverwrite :: IO ()
whenPathExistsNoOverwrite = do
    let env = MockEnv "n" True T.putStr
    runExceptT . flip runReaderT env . runEnv $ do
        archiveM albums
    pure ()

whenPathExistsYesOverwrite :: IO ()
whenPathExistsYesOverwrite = do
    let env = MockEnv "y" True T.putStr
    runExceptT . flip runReaderT env . runEnv $ do
        archiveM albums
    pure ()
