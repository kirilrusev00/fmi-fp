module Image.Dithering.ErrorDiffusion where

import Image
import Data.Word
import Image.PPM

class ErrorDiffusionDitherImage a where
    ditherPixel :: a -> a

    dithered :: a -> Bool

    ditherRow :: Double -> [a] -> [a]

    errorValue :: a -> a -> [Int]

    diffuseError :: a -> [Int] -> Double -> a

---------------------------------------------------------------------------

diffuseColoRError :: Word8 -> Int -> Double -> Word8
diffuseColoRError old err coef
    | new < 0 = 0
    | new > 255 = 255
    | otherwise = floor new
    where new = fromIntegral old + (fromIntegral err) * coef

errValue :: Word8 -> Word8 -> [Int]
errValue oldX newX = [(fromIntegral (oldX) :: Int) - (fromIntegral (newX) :: Int)]

ditherColor :: Word8 -> Word8
ditherColor color = if color < 128 then 0 else 255

ditheredColor :: Word8 -> Bool
ditheredColor x = (x == 0 || x == 255)

---------------------------------------------------------------------------

instance ErrorDiffusionDitherImage Bool where
    ditherPixel pixel = pixel

    dithered pixel = False

    ditherRow coef row = row

    errorValue old new = []

    diffuseError old err coef = old

---------------------------------------------------------------------------

instance ErrorDiffusionDitherImage Word8 where
    ditherPixel x = ditherColor x

    dithered x = ditheredColor x

    ditherRow _ [] = []
    ditherRow _ (x:[]) = [ditherPixel x]
    ditherRow coef (x:y:xs) = newX : ditherRow coef (newY:xs)
        where newX = ditherPixel x
              newY = diffuseError y (errValue x newX) coef

    diffuseError old (err:[]) coef = diffuseColoRError old err coef

    errorValue oldX newX = errValue oldX newX

---------------------------------------------------------------------------

instance ErrorDiffusionDitherImage Rgb where
    ditherPixel rgb = Rgb (ditherColor $ red rgb) (ditherColor $ green rgb) (ditherColor $ blue rgb)

    dithered x = ditheredColor (red x) && ditheredColor (green x) && ditheredColor (blue x)

    ditherRow _ [] = []
    ditherRow _ (x:[]) = [ditherPixel x]
    ditherRow coef (x:y:xs) = newX : ditherRow coef (newY:xs)
        where newX = ditherPixel x
              newY = diffuseError y (errorValue x newX) coef

    diffuseError (Rgb r g b) (errR:errG:errB:[]) coef = Rgb (diffuseColoRError r errR coef) 
                                                            (diffuseColoRError g errG coef)
                                                            (diffuseColoRError b errB coef)

    errorValue oldRgb newRgb = errValue (red oldRgb) (red newRgb) 
                            ++ errValue (green oldRgb) (green newRgb) 
                            ++ errValue (blue oldRgb) (blue newRgb)
