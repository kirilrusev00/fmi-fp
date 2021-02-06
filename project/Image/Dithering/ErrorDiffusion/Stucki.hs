module Image.Dithering.ErrorDiffusion.Stucki where
    
import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

--             X   8   4 
--     2   4   8   4   2
--     1   2   4   2   1
--           (1/42)

class ErrorDiffusionDitherImage a => StuckiDither a where
    ditherSRow :: [a] -> [a] -> [a] -> [[a]]
    ditherSRow [] _ _ = []
    ditherSRow (x:[]) [] [] = [[newX]]
        where newX = ditherPixel x
    ditherSRow (x:y:[]) [] [] = zipWith (:) [newX1] (ditherSRow (newY1:[]) [] [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42)
    ditherSRow (x:y:z:xs) [] [] = zipWith (:) [newX1] (ditherSRow (newY1:newZ1:xs) [] [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42); newZ1 = diffuseError z err1 (4/42)
    ditherSRow (x:[]) (m:[]) []
        | not (dithered x) = [[newX],[newM]]
        | otherwise = [[x],[m]]
        where newX = ditherPixel x; err1 = errorValue x newX
              newM = diffuseError m err1 (8/42)
    ditherSRow (x:y:[]) (m:n:[]) []
        | not (dithered x) = ditherSRow (newX1:newY1:[]) (newM1:newN1:[]) []
        | not (dithered y) = [[x,newY2],[newM2,newN2]]
        | otherwise = [[x,y],[m,n]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42)
              newM1 = diffuseError m err1 (8/42);               newN1 = diffuseError n err1 (4/42)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (4/42);               newN2 = diffuseError n err2 (8/42)
    ditherSRow (x:y:z:[]) (m:n:p:[]) []
        | not (dithered x) = ditherSRow (newX1:newY1:newZ1:[]) (newM1:newN1:newP1:[]) []
        | not (dithered y) = ditherSRow (x:newY2:newZ2:[]) (newM2:newN2:newP2:[]) []
        | otherwise = [[x,y,newZ3],[newM3,newN3,newP3]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42); newZ1 = diffuseError z err1 (4/42)
              newM1 = diffuseError m err1 (8/42);               newN1 = diffuseError n err1 (4/42); newP1 = diffuseError p err1 (2/42)
              newY2 = ditherPixel y; err2 = errorValue y newY2; newZ2 = diffuseError z err2 (8/42)
              newM2 = diffuseError m err2 (4/42); newN2 = diffuseError n err2 (8/42);               newP2 = diffuseError p err2 (4/42)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3
              newM3 = diffuseError m err3 (2/42); newN3 = diffuseError n err3 (4/42); newP3 = diffuseError p err3 (8/42)
    ditherSRow (x:y:z:u:[]) (m:n:p:q:[]) []
        | not (dithered x) = ditherSRow (newX1:newY1:newZ1:u:[]) (newM1:newN1:newP1:q:[]) []
        | not (dithered y) = ditherSRow (x:newY2:newZ2:newU2:[]) (newM2:newN2:newP2:newQ2:[]) []
        | otherwise = zipWith (:) [x,newM3] (ditherSRow (y:newZ3:newU3:[]) (newN3:newP3:newQ3:[]) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42); newZ1 = diffuseError z err1 (4/42)
              newM1 = diffuseError m err1 (8/42);               newN1 = diffuseError n err1 (4/42); newP1 = diffuseError p err1 (2/42)
              newY2 = ditherPixel y; err2 = errorValue y newY2; newZ2 = diffuseError z err2 (8/42); newU2 = diffuseError u err2 (4/42)
              newM2 = diffuseError m err2 (4/42); newN2 = diffuseError n err2 (8/42);               newP2 = diffuseError p err2 (4/42); newQ2 = diffuseError q err2 (2/42)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (8/42)
              newM3 = diffuseError m err3 (2/42); newN3 = diffuseError n err3 (4/42); newP3 = diffuseError p err3 (8/42); newQ3 = diffuseError q err3 (4/42)
    ditherSRow (x:y:z:u:v:xs) (m:n:p:q:r:ys) []
        | not (dithered x) = ditherSRow (newX1:newY1:newZ1:u:v:xs) (newM1:newN1:newP1:q:r:ys) []
        | not (dithered y) = ditherSRow (x:newY2:newZ2:newU2:v:xs) (newM2:newN2:newP2:newQ2:r:ys) []
        | otherwise = zipWith (:) [x,newM3] (ditherSRow (y:newZ3:newU3:newV3:xs) (newN3:newP3:newQ3:newR3:ys) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42); newZ1 = diffuseError z err1 (4/42)
              newM1 = diffuseError m err1 (8/42);               newN1 = diffuseError n err1 (4/42); newP1 = diffuseError p err1 (2/42)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                                     newZ2 = diffuseError z err2 (8/42); newU2 = diffuseError u err2 (4/42)
              newM2 = diffuseError m err2 (4/42); newN2 = diffuseError n err2 (8/42);               newP2 = diffuseError p err2 (4/42); newQ2 = diffuseError q err2 (2/42)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (8/42); newV3 = diffuseError v err3 (4/42)
              newM3 = diffuseError m err3 (2/42); newN3 = diffuseError n err3 (4/42); newP3 = diffuseError p err3 (8/42); newQ3 = diffuseError q err3 (4/42); newR3 = diffuseError r err3 (2/42)
    ditherSRow (x:[]) (m:[]) (d:[])
        | not (dithered x) = ditherSRow (newX1:[]) (newM1:[]) (newD1:[])
        | otherwise = [[x],[m],[d]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1
              newM1 = diffuseError m err1 (8/42)
              newD1 = diffuseError d err1 (4/42)
    ditherSRow (x:y:[]) (m:n:[]) (d:e:[])
        | not (dithered x) = ditherSRow (newX1:newY1:[]) (newM1:newN1:[]) (newD1:newE1:[])
        | not (dithered y) = [[x,newY2],[newM2,newN2],[newD2,newE2]]
        | otherwise = [[x,y],[m,n],[d,e]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42)
              newM1 = diffuseError m err1 (8/42);               newN1 = diffuseError n err1 (4/42)
              newD1 = diffuseError d err1 (4/42);               newE1 = diffuseError e err1 (2/42)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (4/42); newN2 = diffuseError n err2 (8/42)
              newD2 = diffuseError d err2 (2/42); newE2 = diffuseError e err2 (4/42)
    ditherSRow (x:y:z:[]) (m:n:p:[]) (d:e:f:[])
        | not (dithered x) = ditherSRow (newX1:newY1:newZ1:[]) (newM1:newN1:newP1:[]) (newD1:newE1:newF1:[])
        | not (dithered y) = ditherSRow (x:newY2:newZ2:[]) (newM2:newN2:newP2:[]) (newD2:newE2:newF2:[])
        | otherwise = [[x,y,newZ3],[newM3,newN3,newP3],[newD3,newE3,newF3]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42); newZ1 = diffuseError z err1 (4/42)
              newM1 = diffuseError m err1 (8/42);               newN1 = diffuseError n err1 (4/42); newP1 = diffuseError p err1 (2/42)
              newD1 = diffuseError d err1 (4/42);               newE1 = diffuseError e err1 (2/42); newF1 = diffuseError f err1 (1/42)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (8/42)
              newM2 = diffuseError m err2 (4/42); newN2 = diffuseError n err2 (8/42); newP2 = diffuseError p err2 (4/42)
              newD2 = diffuseError d err2 (2/42); newE2 = diffuseError e err2 (4/42); newF2 = diffuseError f err2 (2/42)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3
              newM3 = diffuseError m err3 (2/42); newN3 = diffuseError n err3 (4/42); newP3 = diffuseError p err3 (8/42)
              newD3 = diffuseError d err3 (1/42); newE3 = diffuseError e err3 (2/42); newF3 = diffuseError f err3 (4/42)
    ditherSRow (x:y:z:u:[]) (m:n:p:q:[]) (d:e:f:g:[])
        | not (dithered x) = ditherSRow (newX1:newY1:newZ1:u:[]) (newM1:newN1:newP1:q:[]) (newD1:newE1:newF1:g:[])
        | not (dithered y) = ditherSRow (x:newY2:newZ2:newU2:[]) (newM2:newN2:newP2:newQ2:[]) (newD2:newE2:newF2:newG2:[])
        | otherwise = zipWith (:) [x,newM3,newD3] (ditherSRow (y:newZ3:newU3:[]) (newN3:newP3:newQ3:[]) (newE3:newF3:newG3:[]))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42); newZ1 = diffuseError z err1 (4/42)
              newM1 = diffuseError m err1 (8/42);               newN1 = diffuseError n err1 (4/42); newP1 = diffuseError p err1 (2/42)
              newD1 = diffuseError d err1 (4/42);               newE1 = diffuseError e err1 (2/42); newF1 = diffuseError f err1 (1/42)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (8/42); newU2 = diffuseError u err2 (4/42)
              newM2 = diffuseError m err2 (4/42); newN2 = diffuseError n err2 (8/42); newP2 = diffuseError p err2 (4/42); newQ2 = diffuseError q err2 (2/42)
              newD2 = diffuseError d err2 (2/42); newE2 = diffuseError e err2 (4/42); newF2 = diffuseError f err2 (3/48); newG2 = diffuseError g err2 (1/48)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (8/42)
              newM3 = diffuseError m err3 (2/42); newN3 = diffuseError n err3 (4/42); newP3 = diffuseError p err3 (8/42); newQ3 = diffuseError q err3 (4/42)
              newD3 = diffuseError d err3 (1/42); newE3 = diffuseError e err3 (2/42); newF3 = diffuseError f err3 (4/42); newG3 = diffuseError g err3 (2/42)
    ditherSRow (x:y:z:u:v:xs) (m:n:p:q:r:ys) (d:e:f:g:h:zs)
        | not (dithered x) = ditherSRow (newX1:newY1:newZ1:u:v:xs) (newM1:newN1:newP1:q:r:ys) (newD1:newE1:newF1:g:h:zs)
        | not (dithered y) = ditherSRow (x:newY2:newZ2:newU2:v:xs) (newM2:newN2:newP2:newQ2:r:ys) (newD2:newE2:newF2:newG2:h:zs)
        | otherwise = zipWith (:) [x,newM3,newD3] (ditherSRow (y:newZ3:newU3:newV3:xs) (newN3:newP3:newQ3:newR3:ys) (newE3:newF3:newG3:newH3:zs))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (8/42); newZ1 = diffuseError z err1 (4/42)
              newM1 = diffuseError m err1 (8/42);               newN1 = diffuseError n err1 (4/42); newP1 = diffuseError p err1 (2/42)
              newD1 = diffuseError d err1 (4/42);               newE1 = diffuseError e err1 (2/42); newF1 = diffuseError f err1 (1/42)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (8/42); newU2 = diffuseError u err2 (4/42)
              newM2 = diffuseError m err2 (4/42); newN2 = diffuseError n err2 (8/42); newP2 = diffuseError p err2 (4/42); newQ2 = diffuseError q err2 (2/42)
              newD2 = diffuseError d err2 (2/42); newE2 = diffuseError e err2 (4/42); newF2 = diffuseError f err2 (2/42); newG2 = diffuseError g err2 (1/42)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (8/42); newV3 = diffuseError v err3 (4/42)
              newM3 = diffuseError m err3 (2/42); newN3 = diffuseError n err3 (4/42); newP3 = diffuseError p err3 (8/42); newQ3 = diffuseError q err3 (4/42); newR3 = diffuseError r err3 (2/42)
              newD3 = diffuseError d err3 (1/42); newE3 = diffuseError e err3 (2/42); newF3 = diffuseError f err3 (4/42); newG3 = diffuseError g err3 (2/42); newH3 = diffuseError h err3 (1/42)

    ditherSContents :: [[a]] -> [[a]]
    ditherSContents [] = []
    ditherSContents (x:[]) = ditherSRow x [] []
    ditherSContents (x:y:[]) = ditherSRow x y []
    ditherSContents (x:y:z:xs) = head (ditherSRow x y z) : ditherSContents ((tail (ditherSRow x y z)) ++ xs)

    ditherStucki :: Image a -> Image a
    ditherStucki img = Image (width img) (height img) (ditherSContents (content img))

---------------------------------------------------------------------------

instance StuckiDither Bool where
    ditherStucki img = img

---------------------------------------------------------------------------

instance StuckiDither Word8

---------------------------------------------------------------------------

instance StuckiDither Rgb
