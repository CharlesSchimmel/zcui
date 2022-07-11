
module Zcui.Archive.Class
    ( Archives(..)
    ) where

import           Zcui.Archive.Internal
import           Zcui.Archive.Types
import           Zcui.Class
import           Zcui.Types
import           Zcui.Util

import           Control.Monad.Reader           ( asks )
import           Data.Bifunctor                 ( bimap )
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Text                      ( Text(..) )
import           Prelude                 hiding ( FilePath )
import           Turtle

class Archives m where
  mkTarget :: ArchiveTargetMkr m
  archive :: Archiver m

instance Archives App where
    mkTarget album = do
        opts <- asks $ archiveOptions . config
        getTargetMkr opts album
    archive album = do
        opts <- asks $ archiveOptions . config
        getArchiver opts album

getTargetMkr :: ArchiveOptions -> ArchiveTargetMkr App
getTargetMkr NoArchive _ = pure $ Left "Nothing to do"
getTargetMkr (MoveArchive archDir) album =
    pure . toTextTarget album $ mkMoveTarget archDir album
getTargetMkr (ZipArchive archDir) album =
    pure . toTextTarget album $ mkZipDest archDir album

getArchiver :: ArchiveOptions -> Archiver App
getArchiver NoArchive                = const . pure . pure $ ()
getArchiver (ZipArchive  _         ) = doZip
getArchiver (MoveArchive archiveDir) = doMove
