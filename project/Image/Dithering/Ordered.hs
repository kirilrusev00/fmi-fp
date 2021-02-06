module Image.Dithering.Ordered where

import Image
import Image.PPM
import Data.Word

-- Apply a function f to all elements of a matrix
changeMatrix :: (Int -> Int) -> [[Int]] -> [[Int]]
changeMatrix f matrix = map (map f) matrix

-- If A and B are matrices => the result is A|B 
appendHorMatrices :: [[Int]] -> [[Int]] -> [[Int]]
appendHorMatrices [] [] = []
appendHorMatrices (x:xs) (y:ys) = (x ++ y) : appendHorMatrices xs ys

-- Get the n x n Bayer matrix
bayerMatrix :: Int -> [[Int]]
bayerMatrix n 
    | n == 1 = [[0]]
    | otherwise = upMatrix ++ bottomMatrix  
    where upMatrix = (appendHorMatrices (changeMatrix (4*) (bayerMatrix (n `div` 2))) 
                                     (changeMatrix (\x -> 4 * x + 2) (bayerMatrix (n `div` 2))))
          bottomMatrix = (appendHorMatrices (changeMatrix (\x -> 4 * x + 3) (bayerMatrix (n `div` 2))) 
                                     (changeMatrix (\x -> 4 * x + 1) (bayerMatrix (n `div` 2))))

-- Get the n x n threshold map 
thresholdMap :: (Fractional a) => Int -> [[a]]
thresholdMap n = map (map (\x -> fromIntegral x / fromIntegral (n * n) - 1/2 )) (bayerMatrix n)

--------------------------------------------------------------------------------

class OrderedDitherImage a where
    ditherPixel :: Int -> a -> Int -> Int -> a

    ditherRow :: Int -> [a] -> Int -> Int -> [a]
    ditherRow _ [] _ _ = []
    ditherRow n (x:xs) row col = ditherPixel n x row col : ditherRow n xs row (col+1)

    ditherContents :: Int -> [[a]] -> Int -> [[a]]
    ditherContents _ [] _ = []
    ditherContents n (x:xs) row = ditherRow n x row 0 : ditherContents n xs (row+1)

    ditherBayer :: Int -> Image a -> Image a
    ditherBayer n img = Image (width img) (height img) (ditherContents n (content img) 0)

ditherColor :: Int -> Word8 -> Int -> Int -> Word8
ditherColor n value x y
    | fromIntegral value + r * (thresholdMap n)!!(x `mod` n)!!(y `mod` n) < 127 = 0
    | otherwise = 255
    where r = 255 / fromIntegral(n * n)

--------------------------------------------------------------------------------

ditherBayer4X4 :: (OrderedDitherImage a) => Image a -> Image a
ditherBayer4X4 img = ditherBayer 4 img

ditherBayer8X8 :: (OrderedDitherImage a) => Image a -> Image a
ditherBayer8X8 img = ditherBayer 8 img

---------------------------------------------------------------------------

instance OrderedDitherImage Bool where
    ditherPixel n value x y = undefined

    ditherBayer n img = img

---------------------------------------------------------------------------

instance OrderedDitherImage Word8 where
    ditherPixel n value x y = ditherColor n value x y

---------------------------------------------------------------------------

instance OrderedDitherImage Rgb where
    ditherPixel n rgb x y = Rgb (ditherColor n (red rgb) x y)
                                (ditherColor n (green rgb) x y)
                                (ditherColor n (blue rgb) x y)