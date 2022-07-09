module Main where

import           Lib
import           Options
import           Types

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Functor                   ( ($>) )
import           Data.Text                     as T
                                         hiding ( find
                                                , stripPrefix
                                                )
import           Data.Text.IO                  as T
import           Prelude                       as P
                                         hiding ( FilePath )
import           Turtle

main :: IO ()
main = do
    config <- getConfig
    let env = mkEnv <$> config
    either T.putStrLn runProgram env

runProgram :: Env -> IO ()
runProgram env = do
    runResult <- runExceptT (runReaderT (runApp zcuiM) env)
    either T.putStrLn pure runResult

mkEnv :: Config -> Env
mkEnv conf = Env conf oldLogger

oldLogger :: MonadIO io => Text -> io ()
oldLogger = liftIO . T.putStr
