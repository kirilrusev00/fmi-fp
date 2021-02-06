module Image.Dithering.ErrorDiffusion.Sierra where
    
import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

--            X   5   3
--     2   4  5   4   2
--         2  3   2
--          (1/32)

class ErrorDiffusionDitherImage a => SierraDither a where
    ditherSiRow :: [a] -> [a] -> [a] -> [[a]]
    ditherSiRow [] _ _ = []
    ditherSiRow (x:[]) [] [] = [[newX]]
        where newX = ditherPixel x
    ditherSiRow (x:y:[]) [] [] = zipWith (:) [newX1] (ditherSiRow (newY1:[]) [] [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32)
    ditherSiRow (x:y:z:xs) [] [] = zipWith (:) [newX1] (ditherSiRow (newY1:newZ1:xs) [] [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32); newZ1 = diffuseError z err1 (3/32)
    ditherSiRow (x:[]) (m:[]) []
        | not (dithered x) = [[newX],[newM]]
        | otherwise = [[x],[m]]
        where newX = ditherPixel x; err1 = errorValue x newX
              newM = diffuseError m err1 (5/32)
    ditherSiRow (x:y:[]) (m:n:[]) []
        | not (dithered x) = ditherSiRow (newX1:newY1:[]) (newM1:newN1:[]) []
        | not (dithered y) = [[x,newY2],[newM2,newN2]]
        | otherwise = [[x,y],[m,n]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32)
              newM1 = diffuseError m err1 (5/32);               newN1 = diffuseError n err1 (4/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (5/32)
    ditherSiRow (x:y:z:[]) (m:n:p:[]) []
        | not (dithered x) = ditherSiRow (newX1:newY1:newZ1:[]) (newM1:newN1:newP1:[]) []
        | not (dithered y) = ditherSiRow (x:newY2:newZ2:[]) (newM2:newN2:newP2:[]) []
        | otherwise = zipWith (:) [x,newM3] (ditherSiRow (y:newZ3:[]) (newN3:newP3:[]) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32); newZ1 = diffuseError z err1 (3/32)
              newM1 = diffuseError m err1 (5/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (5/32)
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (5/32); newP2 = diffuseError p err2 (4/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (5/32)
    ditherSiRow (x:y:z:u:[]) (m:n:p:q:[]) []
        | not (dithered x) = ditherSiRow (newX1:newY1:newZ1:u:[]) (newM1:newN1:newP1:q:[]) []
        | not (dithered y) = ditherSiRow (x:newY2:newZ2:newU2:[]) (newM2:newN2:newP2:newQ2:[]) []
        | otherwise = zipWith (:) [x,newM3] (ditherSiRow (y:newZ3:newU3:[]) (newN3:newP3:newQ3:[]) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32); newZ1 = diffuseError z err1 (3/32)
              newM1 = diffuseError m err1 (5/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (5/32); newU2 = diffuseError u err2 (3/32)
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (5/32); newP2 = diffuseError p err2 (4/32); newQ2 = diffuseError q err2 (2/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (5/32)
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (5/32); newQ3 = diffuseError q err3 (4/32)
    ditherSiRow (x:y:z:u:v:xs) (m:n:p:q:r:ys) []
        | not (dithered x) = ditherSiRow (newX1:newY1:newZ1:u:v:xs) (newM1:newN1:newP1:q:r:ys) []
        | not (dithered y) = ditherSiRow (x:newY2:newZ2:newU2:v:xs) (newM2:newN2:newP2:newQ2:r:ys) []
        | otherwise = zipWith (:) [x,newM3] (ditherSiRow (y:newZ3:newU3:newV3:xs) (newN3:newP3:newQ3:newR3:ys) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32); newZ1 = diffuseError z err1 (3/32)
              newM1 = diffuseError m err1 (5/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (5/32); newU2 = diffuseError u err2 (3/32)
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (5/32); newP2 = diffuseError p err2 (4/32); newQ2 = diffuseError q err2 (2/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (5/32); newV3 = diffuseError v err3 (3/32)
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (5/32); newQ3 = diffuseError q err3 (4/32); newR3 = diffuseError r err3 (2/32)
    ditherSiRow (x:[]) (m:[]) (d:[])
        | not (dithered x) = ditherSiRow (newX1:[]) (newM1:[]) (newD1:[])
        | otherwise = [[x],[m],[d]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1
              newM1 = diffuseError m err1 (1/8)
              newD1 = diffuseError d err1 (1/8)
    ditherSiRow (x:y:[]) (m:n:[]) (d:e:[])
        | not (dithered x) = ditherSiRow (newX1:newY1:[]) (newM1:newN1:[]) (newD1:newE1:[])
        | not (dithered y) = [[x,newY2],[newM2,newN2],[newD2,newE2]]
        | otherwise = [[x,y],[m,n],[d,e]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32)
              newM1 = diffuseError m err1 (5/32);               newN1 = diffuseError n err1 (4/32)
              newD1 = diffuseError d err1 (3/32);               newE1 = diffuseError e err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (5/32)
              newD2 = diffuseError d err2 (2/32); newE2 = diffuseError e err2 (3/32)
    ditherSiRow (x:y:z:[]) (m:n:p:[]) (d:e:f:[])
        | not (dithered x) = ditherSiRow (newX1:newY1:newZ1:[]) (newM1:newN1:newP1:[]) (newD1:newE1:f:[])
        | not (dithered y) = ditherSiRow (x:newY2:newZ2:[]) (newM2:newN2:newP2:[]) (newD2:newE2:newF2:[])
        | otherwise = zipWith (:) [x,newM3,d] (ditherSiRow (y:newZ3:[]) (newN3:newP3:[]) (newE3:newF3:[]))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32); newZ1 = diffuseError z err1 (3/32)
              newM1 = diffuseError m err1 (5/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newD1 = diffuseError d err1 (3/32);               newE1 = diffuseError e err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (5/32)
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (5/32); newP2 = diffuseError p err2 (4/32)
              newD2 = diffuseError d err2 (2/32); newE2 = diffuseError e err2 (3/32); newF2 = diffuseError f err2 (2/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (5/32)
              newE3 = diffuseError e err3 (2/32);                                     newF3 = diffuseError f err3 (3/32)
    ditherSiRow (x:y:z:u:[]) (m:n:p:q:[]) (d:e:f:g:[])
        | not (dithered x) = ditherSiRow (newX1:newY1:newZ1:u:[]) (newM1:newN1:newP1:q:[]) (newD1:newE1:f:g:[])
        | not (dithered y) = ditherSiRow (x:newY2:newZ2:newU2:[]) (newM2:newN2:newP2:newQ2:[]) (newD2:newE2:newF2:g:[])
        | otherwise = zipWith (:) [x,newM3,d] (ditherSiRow (y:newZ3:newU3:[]) (newN3:newP3:newQ3:[]) (newE3:newF3:newG3:[]))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32); newZ1 = diffuseError z err1 (3/32)
              newM1 = diffuseError m err1 (5/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newD1 = diffuseError d err1 (3/32);               newE1 = diffuseError e err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (5/32); newU2 = diffuseError u err2 (3/32)
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (5/32); newP2 = diffuseError p err2 (4/32); newQ2 = diffuseError q err2 (2/32)
              newD2 = diffuseError d err2 (2/32); newE2 = diffuseError e err2 (3/32); newF2 = diffuseError f err2 (2/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (5/32)
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (5/32); newQ3 = diffuseError q err3 (4/32)
              newE3 = diffuseError e err3 (2/32);                                     newF3 = diffuseError f err3 (3/32); newG3 = diffuseError g err3 (2/32)
    ditherSiRow (x:y:z:u:v:xs) (m:n:p:q:r:ys) (d:e:f:g:h:zs)
        | not (dithered x) = ditherSiRow (newX1:newY1:newZ1:u:v:xs) (newM1:newN1:newP1:q:r:ys) (newD1:newE1:f:g:h:zs)
        | not (dithered y) = ditherSiRow (x:newY2:newZ2:newU2:v:xs) (newM2:newN2:newP2:newQ2:r:ys) (newD2:newE2:newF2:g:h:zs)
        | otherwise = zipWith (:) [x,newM3,d] (ditherSiRow (y:newZ3:newU3:newV3:xs) (newN3:newP3:newQ3:newR3:ys) (newE3:newF3:newG3:h:zs))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (5/32); newZ1 = diffuseError z err1 (3/32)
              newM1 = diffuseError m err1 (5/32);               newN1 = diffuseError n err1 (4/32); newP1 = diffuseError p err1 (2/32)
              newD1 = diffuseError d err1 (3/32);               newE1 = diffuseError e err1 (2/32)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (5/32); newU2 = diffuseError u err2 (3/32)
              newM2 = diffuseError m err2 (4/32); newN2 = diffuseError n err2 (5/32); newP2 = diffuseError p err2 (4/32); newQ2 = diffuseError q err2 (2/32)
              newD2 = diffuseError d err2 (2/32); newE2 = diffuseError e err2 (3/32); newF2 = diffuseError f err2 (2/32)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (5/32); newV3 = diffuseError v err3 (3/32)
              newM3 = diffuseError m err3 (2/32); newN3 = diffuseError n err3 (4/32); newP3 = diffuseError p err3 (5/32); newQ3 = diffuseError q err3 (4/32); newR3 = diffuseError r err3 (2/32)
              newE3 = diffuseError e err3 (2/32);                                     newF3 = diffuseError f err3 (3/32); newG3 = diffuseError g err3 (2/32)

    ditherSiContents :: [[a]] -> [[a]]
    ditherSiContents [] = []
    ditherSiContents (x:[]) = ditherSiRow x [] []
    ditherSiContents (x:y:[]) = ditherSiRow x y []
    ditherSiContents (x:y:z:xs) = head (ditherSiRow x y z) : ditherSiContents ((tail (ditherSiRow x y z)) ++ xs)

    ditherSierra :: Image a -> Image a
    ditherSierra img = Image (width img) (height img) (ditherSiContents (content img))

---------------------------------------------------------------------------

instance SierraDither Bool where
    ditherSierra img = img

---------------------------------------------------------------------------

instance SierraDither Word8

---------------------------------------------------------------------------

instance SierraDither Rgb

