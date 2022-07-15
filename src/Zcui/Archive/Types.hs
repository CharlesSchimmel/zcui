
module Zcui.Archive.Types where

import           Data.Text                     as T
import           Zcui.Files                     ( FileProjection )
import           Zcui.Types
import           Zcui.Util

type Archiver m = FileProjection -> m (Either Text ())
type ArchiveTargetMkr m = Album -> m (Either Text FileProjection)

