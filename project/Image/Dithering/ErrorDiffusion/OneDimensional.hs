module Image.Dithering.ErrorDiffusion.OneDimensional where

import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

class ErrorDiffusionDitherImage a => OneDimensionalDither a where
    ditherOneDimensionalContents :: [[a]] -> [[a]]
    ditherOneDimensionalContents [] = []
    ditherOneDimensionalContents (x:xs) = ditherRow 1 x : ditherOneDimensionalContents xs

    ditherOneDimensional :: Image a -> Image a
    ditherOneDimensional img = Image (width img) (height img) (ditherOneDimensionalContents (content img))

---------------------------------------------------------------------------

instance OneDimensionalDither Bool where
    ditherOneDimensional img = img

---------------------------------------------------------------------------

instance OneDimensionalDither Word8

---------------------------------------------------------------------------

instance OneDimensionalDither Rgb