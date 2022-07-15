module Zcui.Convert.Class where

import           Zcui.Convert.Types

import           Data.Text
import           Zcui.Files                     ( FileProjection )

class Converts m where
  convertSong :: FileProjection -> m (Either Text ())

