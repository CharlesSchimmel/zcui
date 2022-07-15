module Zcui.Files where

import           Data.Text                      ( Text(..) )
import           Filesystem.Path.CurrentOS
import           Prelude                 hiding ( FilePath )

data FileProjection = FileProjection
    { source :: Text
    , dest   :: Text
    }
    deriving (Show, Eq, Ord)

