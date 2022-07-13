module Zcui.Convert.Class where

import           Zcui.Convert.Types

import           Data.Text

class Converts m where
  convertSong :: ConvertTarget -> m (Either Text ())

