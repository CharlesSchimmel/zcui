module Main where

import           Lib
import           Options
import           Types

import           Control.Monad.Except
import           Data.Functor                   ( ($>) )
import           Control.Monad.Reader
import           Data.Text.IO                  as T
import           Data.Text                     as T
                                         hiding ( find
                                                , stripPrefix
                                                )
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

main :: IO ()
main = do
    args   <- doParseArgs
    optsOk <- verifyOptions args
    let env = mkEnv <$> optsOk
    either T.putStrLn runProgram env

runProgram :: Env -> IO ()
runProgram env = do
    either T.putStrLn pure =<< runExceptT (runReaderT (runApp zcuiM) env)

mkEnv :: Config -> Env
mkEnv conf = Env conf oldLogger

oldLogger :: MonadIO io => Text -> io ()
oldLogger = liftIO . T.putStrLn

verifyOptions :: Arguments -> IO (Either Text Config)
verifyOptions Arguments {..} = do
    music   <- testMusicDir argMusicDir
    archive <- testArchive argArchiveOptions
    let convOpts = testConversionOptions argConversionOptions
    pure $ Config <$> music <*> archive <*> convOpts

testConversionOptions copts@(ConversionOptions (Bitrate bitrate)) =
    if bitrate > 32 && bitrate < 512
        then pure copts
        else Left "Bitrate must be between 32-512"

testMusicDir :: MusicDir -> IO (Either Text MusicDir)
testMusicDir musicDir@(MusicDir path) =
    (fmap $ const musicDir) <$> testdir' path

testArchive :: ArchiveOptions -> IO (Either Text ArchiveOptions)
testArchive NoArchive = pure . pure $ NoArchive
testArchive opts@(MoveArchive (ArchiveDir archDir)) =
    (fmap $ const opts) <$> testdir' archDir
testArchive opts@(ZipArchive (ArchiveDir archDir)) =
    (fmap $ const opts) <$> testdir' archDir

testdir' :: FilePath -> IO (Either Text FilePath)
testdir' path = do
    pathExists <- testdir path
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

