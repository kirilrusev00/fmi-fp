module Image.Dithering.ErrorDiffusion.SierraLite where
    
import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

--            X   2
--        1   1
--          (1/4)

class ErrorDiffusionDitherImage a => SierraLiteDither a where
    ditherSLRow :: [a] -> [a] -> [[a]]
    ditherSLRow [] _ = []
    ditherSLRow row [] = [ditherRow (2/4) row]
    ditherSLRow (x:[]) (m:[])
        | dithered x = [[x],[m]]
        | otherwise = [[newX],[newM]]
        where newX = ditherPixel x; err = errorValue x newX
              newM = diffuseError m err (1/4)
    ditherSLRow (x:y:[]) (m:n:[])
        | dithered x = zipWith (:) [x,newM1] (ditherSLRow (newY1:[]) (newN1:[]))
        | otherwise = ditherSLRow (newX2:newY2:[]) (newM2:n:[])
        where newY1 = ditherPixel y; err1 = errorValue y newY1
              newM1 = diffuseError m err1 (1/4); newN1 = diffuseError n err1 (1/4)
              newX2 = ditherPixel x; err2 = errorValue x newX2; newY2 = diffuseError y err2 (2/4)
              newM2 = diffuseError m err2 (1/4)
    ditherSLRow (x:y:z:xs) (m:n:p:ys) 
        | dithered x = zipWith (:) [x,newM1] (ditherSLRow (newY1:newZ1:xs) (newN1:p:ys))
        | otherwise = ditherSLRow (newX2:newY2:z:xs) (newM2:n:p:ys)
        where newY1 = ditherPixel y; err1 = errorValue y newY1;                                   newZ1 = diffuseError z err1 (2/4)
              newM1 = diffuseError m err1 (1/4);                newN1 = diffuseError n err1 (1/4)
              newX2 = ditherPixel x; err2 = errorValue x newX2; newY2 = diffuseError y err2 (2/4)
              newM2 = diffuseError m err2 (1/4)

    ditherSLContents :: [[a]] -> [[a]]
    ditherSLContents [] = []
    ditherSLContents (x:[]) = ditherSLRow x []
    ditherSLContents (x:y:xs) = head (ditherSLRow x y) : ditherSLContents ((last (ditherSLRow x y)):xs)

    ditherSierraLite :: Image a -> Image a
    ditherSierraLite img = Image (width img) (height img) (ditherSLContents (content img))

---------------------------------------------------------------------------

instance SierraLiteDither Bool where
    ditherSierraLite img = img

---------------------------------------------------------------------------

instance SierraLiteDither Word8

---------------------------------------------------------------------------

instance SierraLiteDither Rgb