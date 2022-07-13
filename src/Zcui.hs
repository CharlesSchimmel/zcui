module Zcui where

import           Zcui.Archive
import           Zcui.Class
import           Zcui.Convert
import           Zcui.Delete
import           Zcui.Find
import           Zcui.Import
import           Zcui.Types
import           Zcui.Util

import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Control.Monad.Trans.Maybe      ( runMaybeT )
import           Data.Functor
import           Data.Text                     as T
import           Prelude                       as P
                                         hiding ( FilePath )

zcui
    :: ( Monad m
       , Finds m
       , Archives m
       , Converts m
       , Deletes m
       , Updates m
       , Imports m
       , MonadReader Env m
       , MonadError Text m
       , Logs m
       , Prompts m
       , TestsPath m
       )
    => m ()
zcui = do
    albums <- findAlbums
    if P.null albums
        then report "No albums found."
        else do
            report_ . T.unlines $ "Found albums:" : P.map artistAlbum albums

            whileAnyItemsLeft $ do
                archived <- itemsLeft albums $ \album -> do
                    report . explainArchiving =<< asks (archiveOptions . config)
                    archiveM album

                converted <- itemsLeft archived $ \album -> do
                    report "Converting"
                    convertM . (>>= songs) $ album

                _ <- itemsLeft converted $ deleteSongsM . P.map originalSong

                _ <- itemsLeft archived $ const updateM

                _ <- itemsLeft archived importM

                return ()
    report "All done :)"
