module Main where

import           Lib
import           Options
import           Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text                     as T
                                         hiding ( find
                                                , stripPrefix
                                                )
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

main :: IO ()
main = do
    opts   <- options "Greetings" optionsParse
    optsOk <- verifyOptions opts
    either print runProgram optsOk

runProgram :: Options -> IO ()
runProgram options = do
    either print pure =<< runExceptT (runReaderT (runApp zcuiM) options)

verifyOptions :: Options -> IO (Either Text Options)
verifyOptions Options {..} = do
    music   <- testMusicDir musicDir
    archive <- testArchive archiveOptions
    pure $ Options <$> music <*> archive

testMusicDir :: MusicDir -> IO (Either Text MusicDir)
testMusicDir musicDir@(MusicDir path) = pure musicDir <$ testdir' path

testArchive :: ArchiveOptions -> IO (Either Text ArchiveOptions)
testArchive NoArchive = pure . pure $ NoArchive
testArchive opts@(MoveArchive (ArchiveDir archDir)) =
    pure opts <$ testdir' archDir
testArchive opts@(ZipArchive (ArchiveDir archDir)) =
    pure opts <$ testdir' archDir

testdir' :: FilePath -> IO (Either Text FilePath)
testdir' = testdir'' testdir

testdir''
    :: Monad m => (FilePath -> m Bool) -> FilePath -> m (Either Text FilePath)
testdir'' tester path = do
    pathExists <- tester path
    if pathExists
        then pure . Right $ path
        else
            pure
            . Left
            . T.append "Could not find path: "
            . T.pack
            . show
            $ path

-- normalizePath :: FilePath -> Shell (Either Text FilePath)
-- normalizePath path
--     | relative path =   (maybe (Left "Failed to absolutize path") Right)
--     <$> homeRelative path
--     | otherwise = pure . pure $ path

-- absolutize :: FilePath -> Shell (Either Text FilePath)
-- absolutize = 

-- absolutize' :: FilePath -> FilePath -> Maybe FilePath
-- absolutize' workingDir targetPath =
--     where withoutDots = strip

-- noDubs :: FilePath -> FilePath
-- noDubs path = maybe path noDubs $ stripPrefix "../" path

-- noDot :: FilePath -> FilePath
-- noDots path = maybe path noDot $ stripPrefix "./" path

-- homeRelative :: FilePath -> Shell (Maybe FilePath)
-- homeRelative path = do
--     home <- need "$HOME"
--     let homePath = fromText <$> home
--     return $ homePath >>= flip homeRelative' path

-- homeRelative' :: FilePath -> FilePath -> Maybe FilePath
-- homeRelative' homeDir targetDir = (homeDir </>) <$> withoutTilde
--     where withoutTilde = stripPrefix "~/" targetDir


