module Image.Dithering.ErrorDiffusion.Atkinson where
    
import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

-- Atkinson algorithm:
--        X   1   1
--    1   1   1
--        1
--      (1/8)

-- The idea is to dither every pixel [p] one by one - so we take the lines containing pixels
-- to which the error from the dithering of [p] is spread and update their values.

-- For each line we take the first pixels which will be affected from the dithering of the first and
-- the second pixel from the line and then continue to the next ones.

-- Each time we take 4 pixels (if there are so many left in the line) 
-- from 3 lines (if there are so many left in the image) and after dithering continue to the next ones.

-- First we check if pixel [x] is dithered:
-- If pixel [x] is not dithered (that means it is the start of the lines),
-- it is dithered, the following errors are spread and we continue to the next pixel
-- x [1/8] : y [1/8] : z [1/8] : u : xs
-- m [1/8] : n [1/8] : p       : q : ys
-- d [1/8] : e       : f       : g : zs
-- If pixel [x] is dithered (that means it is not the start of the lines),
-- it is dithered, the following errors are spread and we continue to the next pixel
-- x       : y [1/8] : z [1/8] : u [1/8] : xs
-- m [1/8] : n [1/8] : p [1/8] : q       : ys
-- d       : e [1/8] : f       : g       : zs

class ErrorDiffusionDitherImage a => AtkinsonDither a where
    ditherARow :: [a] -> [a] -> [a] -> [[a]]
    ditherARow [] _ _ = []
    ditherARow (x:[]) [] [] = [[newX]]
        where newX = ditherPixel x
    ditherARow (x:y:[]) [] [] = zipWith (:) [newX1] (ditherARow (newY1:[]) [] [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (1/8)
    ditherARow (x:y:z:xs) [] [] = zipWith (:) [newX1] (ditherARow (newY1:newZ1:xs) [] [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (1/8); newZ1 = diffuseError z err1 (1/8)
    ditherARow (x:[]) (m:[]) []
        | not (dithered x) = ditherARow (newX1:[]) (newM1:[]) []
        | otherwise = [[x],[m]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1
              newM1 = diffuseError m err1 (1/8)
    ditherARow (x:y:[]) (m:n:[]) []
        | not (dithered x) = ditherARow (newX1:newY1:[]) (newM1:newN1:[]) []
        | otherwise = [[x,newY2],[newM2,newN2]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (1/8)
              newM1 = diffuseError m err1 (1/48);               newN1 = diffuseError n err1 (1/8)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (1/8); newN2 = diffuseError n err2 (1/8)
    ditherARow (x:y:z:[]) (m:n:p:[]) []
        | not (dithered x) = ditherARow (newX1:newY1:newZ1:[]) (newM1:newN1:p:[]) []
        | otherwise = zipWith (:) [x,newM2] (ditherARow (newY2:newZ2:[]) (newN2:newP2:[]) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (1/8); newZ1 = diffuseError z err1 (1/8)
              newM1 = diffuseError m err1 (1/8);                newN1 = diffuseError n err1 (1/8)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                     newZ2 = diffuseError z err2 (1/8)
              newM2 = diffuseError m err2 (1/8); newN2 = diffuseError n err2 (1/8); newP2 = diffuseError p err2 (1/8)
    ditherARow (x:y:z:u:xs) (m:n:p:q:ys) []
        | not (dithered x) = ditherARow (newX1:newY1:newZ1:u:xs) (newM1:newN1:p:q:ys) []
        | otherwise = zipWith (:) [x,newM2] (ditherARow (newY2:newZ2:newU2:xs) (newN2:newP2:q:ys) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (1/8); newZ1 = diffuseError z err1 (1/8)
              newM1 = diffuseError m err1 (1/8);                newN1 = diffuseError n err1 (1/8)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (1/8); newU2 = diffuseError u err2 (1/8)
              newM2 = diffuseError m err2 (1/8); newN2 = diffuseError n err2 (1/8);   newP2 = diffuseError p err2 (1/8)
    ditherARow (x:[]) (m:[]) (d:[])
        | not (dithered x) = ditherARow (newX1:[]) (newM1:[]) (newD1:[])
        | otherwise = [[x],[m],[d]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1
              newM1 = diffuseError m err1 (1/8)
              newD1 = diffuseError d err1 (1/8)
    ditherARow (x:y:[]) (m:n:[]) (d:e:[])
        | not (dithered x) = ditherARow (newX1:newY1:[]) (newM1:newN1:[]) (newD1:e:[])
        | otherwise = [[x,newY2],[newM2,newN2],[d,newE2]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (1/8)
              newM1 = diffuseError m err1 (1/48);               newN1 = diffuseError n err1 (1/8)
              newD1 = diffuseError d err1 (1/8)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (1/8); newN2 = diffuseError n err2 (1/8)
              newE2 = diffuseError e err2 (1/8)
    ditherARow (x:y:z:[]) (m:n:p:[]) (d:e:f:[])
        | not (dithered x) = ditherARow (newX1:newY1:newZ1:[]) (newM1:newN1:p:[]) (newD1:e:f:[])
        | otherwise = zipWith (:) [x,newM2,d] (ditherARow (newY2:newZ2:[]) (newN2:newP2:[]) (newE2:f:[]))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (1/8); newZ1 = diffuseError z err1 (1/8)
              newM1 = diffuseError m err1 (1/8);                newN1 = diffuseError n err1 (1/8)
              newD1 = diffuseError d err1 (1/8)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                     newZ2 = diffuseError z err2 (1/8)
              newM2 = diffuseError m err2 (1/8); newN2 = diffuseError n err2 (1/8); newP2 = diffuseError p err2 (1/8)
              newE2 = diffuseError e err2 (1/8)
    ditherARow (x:y:z:u:xs) (m:n:p:q:ys) (d:e:f:g:zs)
        | not (dithered x) = ditherARow (newX1:newY1:newZ1:u:xs) (newM1:newN1:p:q:ys) (newD1:e:f:g:zs)
        | otherwise = zipWith (:) [x,newM2,d] (ditherARow (newY2:newZ2:newU2:xs) (newN2:newP2:q:ys) (newE2:f:g:zs))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (1/8); newZ1 = diffuseError z err1 (1/8)
              newM1 = diffuseError m err1 (1/8);                newN1 = diffuseError n err1 (1/8)
              newD1 = diffuseError d err1 (1/8)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (1/8); newU2 = diffuseError u err2 (1/8)
              newM2 = diffuseError m err2 (1/8); newN2 = diffuseError n err2 (1/8);   newP2 = diffuseError p err2 (1/8)
              newE2 = diffuseError e err2 (1/8)

    ditherAContents :: [[a]] -> [[a]]
    ditherAContents [] = []
    ditherAContents (x:[]) = ditherARow x [] []
    ditherAContents (x:y:[]) = ditherARow x y []
    ditherAContents (x:y:z:xs) = head (ditherARow x y z) : ditherAContents ((tail (ditherARow x y z)) ++ xs)

    ditherAtkinson :: Image a -> Image a
    ditherAtkinson img = Image (width img) (height img) (ditherAContents (content img))

---------------------------------------------------------------------------

instance AtkinsonDither Bool where
    ditherAtkinson img = img

---------------------------------------------------------------------------

instance AtkinsonDither Word8

---------------------------------------------------------------------------

instance AtkinsonDither Rgb
