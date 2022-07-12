module Zcui.Test.Data where

import           Zcui.Types                     ( Album(..) )

mkAlbum artist albumName = Album { relativePath = "/dev/null"
                                 , absolutePath = "null"
                                 , albumName    = albumName
                                 , artistName   = artist
                                 , songs        = []
                                 }
albums =
    [ mkAlbum "Esquivel"       "Space Age Bachelor Music"
    , mkAlbum "Alice Coltrane" "Journey in Satchidananda"
    , mkAlbum "TR/ST"          "The Destroyer - 1"
    ]


