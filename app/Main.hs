{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Options
import Types

import qualified Control.Foldl as Fold
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Text as T hiding (find)
import Prelude as P hiding (FilePath)
import Turtle

main :: IO ()
main = do
  opts <- options "Greetings" optionsParse
  optsOk <- verifyOptions opts
  either print runProgram optsOk

runProgram :: Options -> IO ()
runProgram options = do
  either print pure =<< runExceptT (runReaderT (runApp zcuiM) options)

verifyOptions :: Options -> IO (Either Text Options)
verifyOptions (Options musicDir archiveOpt) = do
  music <- testMusicDir musicDir
  archive <- testArchive archiveOpt
  pure $ liftM2 Options music archive

testMusicDir :: MusicDir -> IO (Either Text MusicDir)
testMusicDir musicDir@(MusicDir path) = pure musicDir <$ testdir' path

testArchive :: ArchiveOptions -> IO (Either Text ArchiveOptions)
testArchive NoArchive = pure . pure $ NoArchive
testArchive opts@(MoveArchive (ArchiveDir archDir)) =
  pure opts <$ testdir' archDir
testArchive opts@(ZipArchive (ArchiveDir archDir)) =
  pure opts <$ testdir' archDir

testdir' :: FilePath -> IO (Either Text FilePath)
testdir' path = do
  pathExists <- testdir path
  if pathExists
    then pure . Right $ path
    else pure . Left . T.append "Could not find path: " . T.pack . show $ path
