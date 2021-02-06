module Image.Dithering.ErrorDiffusion.TwoRowSierra where
    
import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

--            X   4   3
--    1   2   3   2   1
--          (1/16)

class ErrorDiffusionDitherImage a => TwoRowSierraDither a where
    ditherTRSRow :: [a] -> [a] -> [[a]]
    ditherTRSRow [] _ = []
    ditherTRSRow (x:[]) [] = [[newX]]
        where newX = ditherPixel x
    ditherTRSRow (x:y:[]) [] = zipWith (:) [newX1] (ditherTRSRow (newY1:[]) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (4/16)
    ditherTRSRow (x:y:z:xs) [] = zipWith (:) [newX1] (ditherTRSRow (newY1:newZ1:xs) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (4/16); newZ1 = diffuseError z err1 (3/16)
    ditherTRSRow (x:[]) (m:[])
        | not (dithered x) = [[newX],[newM]]
        | otherwise = [[x],[m]]
        where newX = ditherPixel x; err1 = errorValue x newX
              newM = diffuseError m err1 (3/16)
    ditherTRSRow (x:y:[]) (m:n:[])
        | not (dithered x) = ditherTRSRow (newX1:newY1:[]) (newM1:newN1:[])
        | not (dithered y) = [[x,newY2],[newM2,newN2]]
        | otherwise = [[x,y],[m,n]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (4/16)
              newM1 = diffuseError m err1 (3/16);               newN1 = diffuseError n err1 (2/16)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (2/16); newN2 = diffuseError n err2 (3/16)
    ditherTRSRow (x:y:z:[]) (m:n:p:[])
        | not (dithered x) = ditherTRSRow (newX1:newY1:newZ1:[]) (newM1:newN1:newP1:[])
        | not (dithered y) = ditherTRSRow (x:newY2:newZ2:[]) (newM2:newN2:newP2:[])
        | otherwise = [[x,y,newZ3],[newM3,newN3,newP3]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (4/16); newZ1 = diffuseError z err1 (3/16)
              newM1 = diffuseError m err1 (3/16);               newN1 = diffuseError n err1 (2/16); newP1 = diffuseError p err1 (1/16)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (4/16)
              newM2 = diffuseError m err2 (2/16); newN2 = diffuseError n err2 (3/16); newP2 = diffuseError p err2 (2/16)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3
              newM3 = diffuseError m err3 (1/16); newN3 = diffuseError n err3 (2/16); newP3 = diffuseError p err3 (3/16)
    ditherTRSRow (x:y:z:u:[]) (m:n:p:q:[])
        | not (dithered x) = ditherTRSRow (newX1:newY1:newZ1:u:[]) (newM1:newN1:newP1:q:[])
        | not (dithered y) = ditherTRSRow (x:newY2:newZ2:newU2:[]) (newM2:newN2:newP2:newQ2:[])
        | otherwise = zipWith (:) [x,newM3] (ditherTRSRow (y:newZ3:newU3:[]) (newN3:newP3:newQ3:[]))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (4/16); newZ1 = diffuseError z err1 (3/16)
              newM1 = diffuseError m err1 (3/16);               newN1 = diffuseError n err1 (2/16); newP1 = diffuseError p err1 (1/16)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (4/16); newU2 = diffuseError u err2 (3/16)
              newM2 = diffuseError m err2 (2/16); newN2 = diffuseError n err2 (3/16); newP2 = diffuseError p err2 (2/16); newQ2 = diffuseError q err2 (1/16)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (4/16)
              newM3 = diffuseError m err3 (1/16); newN3 = diffuseError n err3 (2/16); newP3 = diffuseError p err3 (3/16); newQ3 = diffuseError q err3 (2/16)
    ditherTRSRow (x:y:z:u:v:xs) (m:n:p:q:r:ys) 
        | not (dithered x) = ditherTRSRow (newX1:newY1:newZ1:u:v:xs) (newM1:newN1:newP1:q:r:ys)
        | not (dithered y) = ditherTRSRow (x:newY2:newZ2:newU2:v:xs) (newM2:newN2:newP2:newQ2:r:ys)
        | otherwise = zipWith (:) [x,newM3] (ditherTRSRow (y:newZ3:newU3:newV3:xs) 
                                                         (newN3:newP3:newQ3:newR3:ys))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (4/16); newZ1 = diffuseError z err1 (3/16)
              newM1 = diffuseError m err1 (3/16);               newN1 = diffuseError n err1 (2/16); newP1 = diffuseError p err1 (1/16)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (4/16); newU2 = diffuseError u err2 (3/16)
              newM2 = diffuseError m err2 (2/16); newN2 = diffuseError n err2 (3/16); newP2 = diffuseError p err2 (2/16); newQ2 = diffuseError q err2 (1/16)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (4/16); newV3 = diffuseError v err3 (3/16)
              newM3 = diffuseError m err3 (1/16); newN3 = diffuseError n err3 (2/16); newP3 = diffuseError p err3 (3/16); newQ3 = diffuseError q err3 (2/16); newR3 = diffuseError r err3 (1/16)

    ditherTRSContents :: [[a]] -> [[a]]
    ditherTRSContents [] = []
    ditherTRSContents (x:[]) = ditherTRSRow x []
    ditherTRSContents (x:y:xs) = head (ditherTRSRow x y) : 
                                                ditherTRSContents ((last (ditherTRSRow x y)):xs)

    ditherTwoRowSierra :: Image a -> Image a
    ditherTwoRowSierra img = Image (width img) (height img) (ditherTRSContents (content img))

---------------------------------------------------------------------------

instance TwoRowSierraDither Bool where
    ditherTwoRowSierra img = img

---------------------------------------------------------------------------

instance TwoRowSierraDither Word8

---------------------------------------------------------------------------

instance TwoRowSierraDither Rgb