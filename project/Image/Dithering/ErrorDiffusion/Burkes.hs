module Image.Dithering.ErrorDiffusion.Burkes where
    
import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

--             X   8   4 
--     2   4   8   4   2
--           (1/32)

class ErrorDiffusionDitherImage a => BurkesDither a where
    ditherBRow :: [a] -> [a] -> [[a]]
    ditherBRow [] _ = []
    ditherBRow (x:[]) [] = [[newX]]
        where newX = ditherPixel x
    ditherBRow (x:y:[]) [] = zipWith (:) [newX1] (ditherBRow (newY1:[]) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/32)
    ditherBRow (x:y:z:xs) [] = zipWith (:) [newX1] (ditherBRow (newY1:newZ1:xs) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/32); newZ1 = diffuseError z err1 (4/32)
    ditherBRow (x:[]) (m:[])
        | not (dithered x) = [[newX],[newM]]
        | otherwise = [[x],[m]]
        where newX = ditherPixel x; err1 = errorValue x newX
              newM = diffuseError m err1 (8/32)
    ditherBRow (x:y:[]) (m:n:[])
        | not (dithered x) = ditherBRow (newX1:newY1:[]) (newM1:newN1:[])
        | not (dithered y) = [[x,newY2],[newM2,newN2]]
        | otherwise = [[x,y],[m,n]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/32)
              newM1 = diffuseError m err1 (8/32);               newN1 = diffuseError n err1 (4/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (8/32)
    ditherBRow (x:y:z:[]) (m:n:p:[])
        | not (dithered x) = ditherBRow (newX1:newY1:newZ1:[]) (newM1:newN1:newP1:[])
        | not (dithered y) = ditherBRow (x:newY2:newZ2:[]) (newM2:newN2:newP2:[])
        | otherwise = [[x,y,newZ3],[newM3,newN3,newP3]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/32); newZ1 = diffuseError z err1 (4/32)
              newM1 = diffuseError m err1 (8/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2; newZ2 = diffuseError z err2 (8/32)
              newM2 = diffuseError m err2 (4/32);               newN2 = diffuseError n err2 (8/32); newP2 = diffuseError p err2 (4/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (8/32)
    ditherBRow (x:y:z:u:[]) (m:n:p:q:[])
        | not (dithered x) = ditherBRow (newX1:newY1:newZ1:u:[]) (newM1:newN1:newP1:q:[])
        | not (dithered y) = ditherBRow (x:newY2:newZ2:newU2:[]) (newM2:newN2:newP2:newQ2:[])
        | otherwise = zipWith (:) [x,newM3] (ditherBRow (y:newZ3:newU3:[]) 
                                                         (newN3:newP3:newQ3:[]))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/32); newZ1 = diffuseError z err1 (4/32)
              newM1 = diffuseError m err1 (8/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                                     newZ2 = diffuseError z err2 (8/32); newU2 = diffuseError u err2 (4/32)
              newM2 = diffuseError m err2 (4/32);               newN2 = diffuseError n err2 (8/32); newP2 = diffuseError p err2 (4/32); newQ2 = diffuseError q err2 (2/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (8/32)
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (8/32); newQ3 = diffuseError q err3 (4/32)
    ditherBRow (x:y:z:u:v:xs) (m:n:p:q:r:ys) 
        | not (dithered x) = ditherBRow (newX1:newY1:newZ1:u:v:xs) (newM1:newN1:newP1:q:r:ys)
        | not (dithered y) = ditherBRow (x:newY2:newZ2:newU2:v:xs) (newM2:newN2:newP2:newQ2:r:ys)
        | otherwise = zipWith (:) [x,newM3] (ditherBRow (y:newZ3:newU3:newV3:xs) 
                                                         (newN3:newP3:newQ3:newR3:ys))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/32); newZ1 = diffuseError z err1 (4/32)
              newM1 = diffuseError m err1 (8/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (8/32); newU2 = diffuseError u err2 (4/32)
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (8/32); newP2 = diffuseError p err2 (4/32); newQ2 = diffuseError q err2 (2/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (8/32); newV3 = diffuseError v err3 (4/32)
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (8/32); newQ3 = diffuseError q err3 (4/32); newR3 = diffuseError r err3 (2/32)

    ditherBContents :: [[a]] -> [[a]]
    ditherBContents [] = []
    ditherBContents (x:[]) = ditherBRow x []
    ditherBContents (x:y:xs) = head (ditherBRow x y) : 
                                                ditherBContents ((last (ditherBRow x y)):xs)

    ditherBurkes :: Image a -> Image a
    ditherBurkes img = Image (width img) (height img) (ditherBContents (content img))

---------------------------------------------------------------------------

instance BurkesDither Bool where
    ditherBurkes img = img

---------------------------------------------------------------------------

instance BurkesDither Word8

---------------------------------------------------------------------------

instance BurkesDither Rgb
