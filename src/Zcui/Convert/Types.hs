module Zcui.Convert.Types where

import           Zcui.Types

import           Data.Text

data ConvertedSong = ConvertedSong
    { originalSong  :: Song
    , convertedSong :: Song
    }
    deriving (Show, Eq, Ord)

