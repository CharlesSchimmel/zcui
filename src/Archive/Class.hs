
module Archive.Class where

import           Archive.Types
import           Class
import           Types

import           Data.Maybe
import           Data.Text                      ( Text(..) )
import           Prelude                 hiding ( FilePath )
import           Turtle

class Zips m where
  zip :: ArchiveTarget -> m (Either Text ())

class Moves m where
  move :: ArchiveTarget -> m (Either Text ())

class Archives m where
  mkTarget :: ArchiveTargetMkr m
  archive :: Archiver m

