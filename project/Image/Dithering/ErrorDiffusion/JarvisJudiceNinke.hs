module Image.Dithering.ErrorDiffusion.JarvisJudiceNinke where
    
import Image.Dithering.ErrorDiffusion
import Data.Word
import Image.PPM
import Image

--             X   7   5 
--     3   5   7   5   3
--     1   3   5   3   1
--           (1/48)

class ErrorDiffusionDitherImage a => JarvisJudiceNinkeDither a where
    ditherJJNRow :: [a] -> [a] -> [a] -> [[a]]
    ditherJJNRow [] _ _ = []
    ditherJJNRow (x:[]) [] [] = [[newX]]
        where newX = ditherPixel x
    ditherJJNRow (x:y:[]) [] [] = zipWith (:) [newX1] (ditherJJNRow (newY1:[]) [] [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48)
    ditherJJNRow (x:y:z:xs) [] [] = zipWith (:) [newX1] (ditherJJNRow (newY1:newZ1:xs) [] [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48); newZ1 = diffuseError z err1 (5/48)
    ditherJJNRow (x:[]) (m:[]) []
        | not (dithered x) = [[newX],[newM]]
        | otherwise = [[x],[m]]
        where newX = ditherPixel x; err1 = errorValue x newX
              newM = diffuseError m err1 (7/48)
    ditherJJNRow (x:y:[]) (m:n:[]) []
        | not (dithered x) = ditherJJNRow (newX1:newY1:[]) (newM1:newN1:[]) []
        | not (dithered y) = [[x,newY2],[newM2,newN2]]
        | otherwise = [[x,y],[m,n]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48)
              newM1 = diffuseError m err1 (7/48);               newN1 = diffuseError n err1 (5/48)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (5/48); newN2 = diffuseError n err2 (7/48)
    ditherJJNRow (x:y:z:[]) (m:n:p:[]) []
        | not (dithered x) = ditherJJNRow (newX1:newY1:newZ1:[]) (newM1:newN1:newP1:[]) []
        | not (dithered y) = ditherJJNRow (x:newY2:newZ2:[]) (newM2:newN2:newP2:[]) []
        | otherwise = [[x,y,newZ3],[newM3,newN3,newP3]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48); newZ1 = diffuseError z err1 (5/48)
              newM1 = diffuseError m err1 (7/48);               newN1 = diffuseError n err1 (5/48); newP1 = diffuseError p err1 (3/48)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (7/48)
              newM2 = diffuseError m err2 (5/48); newN2 = diffuseError n err2 (7/48); newP2 = diffuseError p err2 (5/48)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3
              newM3 = diffuseError m err3 (3/48); newN3 = diffuseError n err3 (5/48); newP3 = diffuseError p err3 (7/48)
    ditherJJNRow (x:y:z:u:[]) (m:n:p:q:[]) []
        | not (dithered x) = ditherJJNRow (newX1:newY1:newZ1:u:[]) (newM1:newN1:newP1:q:[]) []
        | not (dithered y) = ditherJJNRow (x:newY2:newZ2:newU2:[]) (newM2:newN2:newP2:newQ2:[]) []
        | otherwise = zipWith (:) [x,newM3] (ditherJJNRow (y:newZ3:newU3:[]) (newN3:newP3:newQ3:[]) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48); newZ1 = diffuseError z err1 (5/48)
              newM1 = diffuseError m err1 (7/48);               newN1 = diffuseError n err1 (5/48); newP1 = diffuseError p err1 (3/48)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (7/48); newU2 = diffuseError u err2 (5/48)
              newM2 = diffuseError m err2 (5/48); newN2 = diffuseError n err2 (7/48); newP2 = diffuseError p err2 (5/48); newQ2 = diffuseError q err2 (3/48)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (7/48)
              newM3 = diffuseError m err3 (3/48); newN3 = diffuseError n err3 (5/48); newP3 = diffuseError p err3 (7/48); newQ3 = diffuseError q err3 (5/48)
    ditherJJNRow (x:y:z:u:v:xs) (m:n:p:q:r:ys) []
        | not (dithered x) = ditherJJNRow (newX1:newY1:newZ1:u:v:xs) (newM1:newN1:newP1:q:r:ys) []
        | not (dithered y) = ditherJJNRow (x:newY2:newZ2:newU2:v:xs) (newM2:newN2:newP2:newQ2:r:ys) []
        | otherwise = zipWith (:) [x,newM3] (ditherJJNRow (y:newZ3:newU3:newV3:xs) (newN3:newP3:newQ3:newR3:ys) [])
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48); newZ1 = diffuseError z err1 (5/48)
              newM1 = diffuseError m err1 (7/48);               newN1 = diffuseError n err1 (5/48); newP1 = diffuseError p err1 (3/48)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (7/48); newU2 = diffuseError u err2 (5/48)
              newM2 = diffuseError m err2 (5/48); newN2 = diffuseError n err2 (7/48); newP2 = diffuseError p err2 (5/48); newQ2 = diffuseError q err2 (3/48)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (7/48); newV3 = diffuseError v err3 (5/48)
              newM3 = diffuseError m err3 (3/48); newN3 = diffuseError n err3 (5/48); newP3 = diffuseError p err3 (7/48); newQ3 = diffuseError q err3 (5/48); newR3 = diffuseError r err3 (3/48)
    ditherJJNRow (x:[]) (m:[]) (d:[])
        | not (dithered x) = ditherJJNRow (newX1:[]) (newM1:[]) (newD1:[])
        | otherwise = [[x],[m],[d]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newM1 = diffuseError m err1 (7/48); newD1 = diffuseError d err1 (5/48)
    ditherJJNRow (x:y:[]) (m:n:[]) (d:e:[])
        | not (dithered x) = ditherJJNRow (newX1:newY1:[]) (newM1:newN1:[]) (newD1:newE1:[])
        | not (dithered y) = [[x,newY2],[newM2,newN2],[newD2,newE2]]
        | otherwise = [[x,y],[m,n],[d,e]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48)
              newM1 = diffuseError m err1 (7/48);               newN1 = diffuseError n err1 (5/48)
              newD1 = diffuseError d err1 (5/48);               newE1 = diffuseError e err1 (3/48)
              newY2 = ditherPixel y; err2 = errorValue y newY2
              newM2 = diffuseError m err2 (5/48); newN2 = diffuseError n err2 (7/48)
              newD2 = diffuseError d err2 (3/48); newE2 = diffuseError e err2 (5/48)
    ditherJJNRow (x:y:z:[]) (m:n:p:[]) (d:e:f:[])
        | not (dithered x) = ditherJJNRow (newX1:newY1:newZ1:[]) (newM1:newN1:newP1:[]) (newD1:newE1:newF1:[])
        | not (dithered y) = ditherJJNRow (x:newY2:newZ2:[]) (newM2:newN2:newP2:[]) (newD2:newE2:newF2:[])
        | otherwise = [[x,y,newZ3],[newM3,newN3,newP3],[newD3,newE3,newF3]]
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48); newZ1 = diffuseError z err1 (5/48)
              newM1 = diffuseError m err1 (7/48);               newN1 = diffuseError n err1 (5/48); newP1 = diffuseError p err1 (3/48)
              newD1 = diffuseError d err1 (5/48);               newE1 = diffuseError e err1 (3/48); newF1 = diffuseError f err1 (1/48)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (7/48)
              newM2 = diffuseError m err2 (5/48); newN2 = diffuseError n err2 (7/48); newP2 = diffuseError p err2 (5/48)
              newD2 = diffuseError d err2 (3/48); newE2 = diffuseError e err2 (5/48); newF2 = diffuseError f err2 (3/48)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3
              newM3 = diffuseError m err3 (3/48); newN3 = diffuseError n err3 (5/48); newP3 = diffuseError p err3 (7/48)
              newD3 = diffuseError d err3 (1/48); newE3 = diffuseError e err3 (3/48); newF3 = diffuseError f err3 (5/48)
    ditherJJNRow (x:y:z:u:[]) (m:n:p:q:[]) (d:e:f:g:[])
        | not (dithered x) = ditherJJNRow (newX1:newY1:newZ1:u:[]) (newM1:newN1:newP1:q:[]) (newD1:newE1:newF1:g:[])
        | not (dithered y) = ditherJJNRow (x:newY2:newZ2:newU2:[]) (newM2:newN2:newP2:newQ2:[]) (newD2:newE2:newF2:newG2:[])
        | otherwise = zipWith (:) [x,newM3,newD3] (ditherJJNRow (y:newZ3:newU3:[]) (newN3:newP3:newQ3:[]) (newE3:newF3:newG3:[]))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48); newZ1 = diffuseError z err1 (5/48)
              newM1 = diffuseError m err1 (7/48);               newN1 = diffuseError n err1 (5/48); newP1 = diffuseError p err1 (3/48)
              newD1 = diffuseError d err1 (5/48);               newE1 = diffuseError e err1 (3/48); newF1 = diffuseError f err1 (1/48)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                       newZ2 = diffuseError z err2 (7/48); newU2 = diffuseError u err2 (5/48)
              newM2 = diffuseError m err2 (5/48); newN2 = diffuseError n err2 (7/48); newP2 = diffuseError p err2 (5/48); newQ2 = diffuseError q err2 (3/48)
              newD2 = diffuseError d err2 (3/48); newE2 = diffuseError e err2 (5/48); newF2 = diffuseError f err2 (3/48); newG2 = diffuseError g err2 (1/48)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (7/48)
              newM3 = diffuseError m err3 (3/48); newN3 = diffuseError n err3 (5/48); newP3 = diffuseError p err3 (7/48); newQ3 = diffuseError q err3 (5/48)
              newD3 = diffuseError d err3 (1/48); newE3 = diffuseError e err3 (3/48); newF3 = diffuseError f err3 (5/48); newG3 = diffuseError g err3 (3/48)
    ditherJJNRow (x:y:z:u:v:xs) (m:n:p:q:r:ys) (d:e:f:g:h:zs)
        | not (dithered x) = ditherJJNRow (newX1:newY1:newZ1:u:v:xs) (newM1:newN1:newP1:q:r:ys) (newD1:newE1:newF1:g:h:zs)
        | not (dithered y) = ditherJJNRow (x:newY2:newZ2:newU2:v:xs) (newM2:newN2:newP2:newQ2:r:ys) (newD2:newE2:newF2:newG2:h:zs)
        | otherwise = zipWith (:) [x,newM3,newD3] (ditherJJNRow (y:newZ3:newU3:newV3:xs) (newN3:newP3:newQ3:newR3:ys) (newE3:newF3:newG3:newH3:zs))
        where newX1 = ditherPixel x; err1 = errorValue x newX1; newY1 = diffuseError y err1 (7/48); newZ1 = diffuseError z err1 (5/48)
              newM1 = diffuseError m err1 (7/48);               newN1 = diffuseError n err1 (5/48); newP1 = diffuseError p err1 (3/48)
              newD1 = diffuseError d err1 (5/48);               newE1 = diffuseError e err1 (3/48); newF1 = diffuseError f err1 (1/48)
              newY2 = ditherPixel y; err2 = errorValue y newY2;                                     newZ2 = diffuseError z err2 (7/48); newU2 = diffuseError u err2 (5/48)
              newM2 = diffuseError m err2 (5/48);               newN2 = diffuseError n err2 (7/48); newP2 = diffuseError p err2 (5/48); newQ2 = diffuseError q err2 (3/48)
              newD2 = diffuseError d err2 (3/48);               newE2 = diffuseError e err2 (5/48); newF2 = diffuseError f err2 (3/48); newG2 = diffuseError g err2 (1/48)
              newZ3 = ditherPixel z; err3 = errorValue z newZ3;                                                           newU3 = diffuseError u err3 (7/48); newV3 = diffuseError v err3 (5/48)
              newM3 = diffuseError m err3 (3/48); newN3 = diffuseError n err3 (5/48); newP3 = diffuseError p err3 (7/48); newQ3 = diffuseError q err3 (5/48); newR3 = diffuseError r err3 (3/48)
              newD3 = diffuseError d err3 (1/48); newE3 = diffuseError e err3 (3/48); newF3 = diffuseError f err3 (5/48); newG3 = diffuseError g err3 (3/48); newH3 = diffuseError h err3 (1/48)

    ditherJJNContents :: [[a]] -> [[a]]
    ditherJJNContents [] = []
    ditherJJNContents (x:[]) = ditherJJNRow x [] []
    ditherJJNContents (x:y:[]) = ditherJJNRow x y []
    ditherJJNContents (x:y:z:xs) = head (ditherJJNRow x y z) : ditherJJNContents ((tail (ditherJJNRow x y z)) ++ xs)

    ditherJarvisJudiceNinke :: Image a -> Image a
    ditherJarvisJudiceNinke img = Image (width img) (height img) (ditherJJNContents (content img))

---------------------------------------------------------------------------

instance JarvisJudiceNinkeDither Bool where
    ditherJarvisJudiceNinke img = img

---------------------------------------------------------------------------

instance JarvisJudiceNinkeDither Word8

---------------------------------------------------------------------------

instance JarvisJudiceNinkeDither Rgb
