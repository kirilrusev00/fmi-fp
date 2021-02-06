module Image.PPM where

import Image
import Data.Word

data Rgb = Rgb { red :: Word8
               , green :: Word8
               , blue :: Word8 } deriving (Show,Read)
