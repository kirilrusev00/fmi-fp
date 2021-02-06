module Image.Dithering.ErrorDiffusion.FloydSteinberg where

import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

--       X   7
--   3   5   1
--     (1/16)

class ErrorDiffusionDitherImage a => FloydSteinbergDither a where
    ditherFSRow :: [a] -> [a] -> [[a]]
    ditherFSRow [] _ = []
    ditherFSRow row [] = [ditherRow (7/16) row]
    ditherFSRow (x:[]) (m:[])
        | dithered x = [[x],[m]]
        | otherwise = [[newX],[newM]]
        where newX = ditherPixel x; err = errorValue x newX; newM = diffuseError m err (5/16)
    ditherFSRow (x:y:[]) (m:n:[])
        | dithered x = zipWith (:) [x,newM1] (ditherFSRow (newY1:[]) (newN1:[]))
        | otherwise = ditherFSRow (newX2:newY2:[]) (newM2:newN2:[])
        where newY1 = ditherPixel y; err1 = errorValue y newY1
              newM1 = diffuseError m err1 (3/16); newN1 = diffuseError n err1 (5/16)
              newX2 = ditherPixel x; err2 = errorValue x newX2; newY2 = diffuseError y err2 (7/16)
              newM2 = diffuseError m err2 (5/16);               newN2 = diffuseError n err2 (1/16)
    ditherFSRow (x:y:z:xs) (m:n:p:ys) 
        | dithered x = zipWith (:) [x,newM1] (ditherFSRow (newY1:newZ1:xs) (newN1:newP1:ys))
        | otherwise = ditherFSRow (newX2:newY2:z:xs) (newM2:newN2:p:ys)
        where newY1 = ditherPixel y; err1 = errorValue y newY1;                       newZ1 = diffuseError z err1 (7/16)
              newM1 = diffuseError m err1 (3/16); newN1 = diffuseError n err1 (5/16); newP1 = diffuseError p err1 (1/16)
              newX2 = ditherPixel x; err2 = errorValue x newX2; newY2 = diffuseError y err2 (7/16)
              newM2 = diffuseError m err2 (5/16);               newN2 = diffuseError n err2 (1/16)

    ditherFSContents :: [[a]] -> [[a]]
    ditherFSContents [] = []
    ditherFSContents (x:[]) = ditherFSRow x []
    ditherFSContents (x:y:xs) = head (ditherFSRow x y) : ditherFSContents ((last (ditherFSRow x y)):xs)

    ditherFS :: Image a -> Image a
    ditherFS img = Image (width img) (height img) (ditherFSContents (content img))

---------------------------------------------------------------------------

instance FloydSteinbergDither Bool where
    ditherFS img = img

---------------------------------------------------------------------------

instance FloydSteinbergDither Word8

---------------------------------------------------------------------------

instance FloydSteinbergDither Rgb