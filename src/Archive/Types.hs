
module Archive.Types where

import           Data.Text                     as T
import           Types
import           Util

type Archiver m = ArchiveTarget -> m (Either Text ())
type ArchiveTargetMkr m = Album -> m (Either Text ArchiveTarget)

data ArchiveTarget = ArchiveTarget
    { source :: Text
    , dest   :: Text
    }

