module Task2 where

import Data.Word

data Rgb = Rgb { red :: Word8
               , green :: Word8
               , blue :: Word8 } deriving (Show,Read)

instance Eq Rgb where
    (Rgb r1 g1 b1) == (Rgb r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

data Image = Image { width :: Int
                   , height :: Int
                   , content :: [[Rgb]] } deriving (Show,Read)

instance Eq Image where
    (Image w1 h1 c1) == (Image w2 h2 c2) = w1 == w2 && h1 == h2 && c1 == c2

emptyImage :: Image
emptyImage = Image 0 0 []
 
convertWord8ToRgb :: Word8 -> Rgb
convertWord8ToRgb word = Rgb word word word

-- А

grayscaleRow :: [Rgb] -> [Rgb]
grayscaleRow [] = []
grayscaleRow (p:pxs) = convertWord8ToRgb (grayscaleValue p) : grayscaleRow pxs
    where grayscaleValue p = floor (0.30 * (fromIntegral $ red p) + 0.59 * (fromIntegral $ green p) + 0.11 * (fromIntegral $ blue p))

grayscaleContent :: [[Rgb]] -> [[Rgb]]
grayscaleContent [] = []
grayscaleContent (row:img) = grayscaleRow row : grayscaleContent img

grayscale :: Image -> Image
grayscale img = Image (width img) (height img) (grayscaleContent $ content img)

-- Б

-- use Extend Edge Handling strategy to get pixels outside the image
getExtendedPixelValue :: Int -> Int -> Image -> Int
getExtendedPixelValue row col img = fromIntegral $ red $ (content img)!!x!!y :: Int
    where x = if row < 0 then 0 
                         else if row >= height img then (height img) - 1 
                                                   else row
          y = if col < 0 then 0 
                         else if col >= width img then (width img) - 1 
                                                  else col

formMatrix :: Image -> Int -> Int -> [[Int]]
formMatrix img row col = [value (row-1) (col-1) : value (row-1) col : [value (row-1) (col+1)],
                          value row (col-1) : value row col : [value row (col+1)],
                          value (row+1) (col-1) : value (row+1) col : [value (row+1) (col+1)]]
    where value row col = getExtendedPixelValue row col img

convolution :: [[Int]] -> [[Int]] -> Int
convolution m k = m!!2!!2 * k!!0!!0 + m!!2!!1 * k!!0!!1 + m!!2!!0 * k!!0!!2 
                + m!!1!!2 * k!!1!!0 + m!!1!!1 * k!!1!!1 + m!!1!!0 * k!!1!!2
                + m!!0!!2 * k!!2!!0 + m!!0!!1 * k!!2!!1 + m!!0!!0 * k!!2!!2

sobelOperator :: [[Int]] -> Word8
sobelOperator matrix
    | grayscaleEdgeValue < 0 = 0
    | grayscaleEdgeValue > 255 = 255
    | otherwise = floor grayscaleEdgeValue
    where grayscaleEdgeValue = sqrt (firstResult * firstResult + secondResult * secondResult)
          firstResult = fromIntegral $ convolution matrix [[1,0,-1],[2,0,-2],[1,0,-1]]
          secondResult = fromIntegral $ convolution matrix [[1,2,1],[0,0,0],[-1,-2,-1]]

isGrayscaleImg :: Image -> Bool
isGrayscaleImg img = foldl (\result row -> result && isGrayscaleRow row) True (content img)
    where isGrayscaleRow row = foldl (\result rgb -> result && green rgb == red rgb && green rgb == blue rgb) True row

edgeDetect :: Image -> Image
edgeDetect img 
    | isGrayscaleImg img = Image w h edgeDetectContent
    | otherwise = img
    where w = width img
          h = height img
          edgeDetectContent = [[ result row col | col <- [0..w-1] ] | row <- [0..h-1]]
          result row col = convertWord8ToRgb $ sobelOperator $ formMatrix img row col

-- В

getColor :: Int -> Int -> Image -> Rgb
getColor x y img = (content img)!!y!!x

{-|
    Algorithm for floodFillHelper 
    - initColor is the color of the first pixel [the one for which floodFill was called]
    - newColor is the color with which the image is filled
    - x and y are the coordinates of the current pixel in the image img

    1) if initColor == newColor, return img
    2) if the color of the current pixel is not equal to initColor, return img
    3) set the color of the current pixel to newColor.
    4) call floodFillHelper for all of the neighbour pixels (up, down, left and right) of the current pixel 
       and return the result
-}

floodFillHelper :: Rgb -> Rgb -> Int -> Int -> Image -> Image
floodFillHelper initColor newColor x y img
    | x < 0 || y < 0 || x > w - 1 || y > h - 1 = img
    | initColor == newColor = img
    | initColor /= color x y = img
    | otherwise = fillLeft $ fillRight $ fillUp $ fillDown changeColor
    where w = width img
          h = height img
          color x y = getColor x y img
          fillNeighbour x y img = floodFillHelper initColor newColor x y img
          fillDown img = fillNeighbour x (y+1) img
          fillUp img = fillNeighbour x (y-1) img
          fillRight img = fillNeighbour (x+1) y img
          fillLeft img = fillNeighbour (x-1) y img
          changeColor = Image w h [[ if row == y && col == x 
                                        then newColor
                                        else color col row | col <- [0..w-1] ] | row <- [0..h-1]]

floodFill :: Rgb -> Int -> Int -> Image -> Image
floodFill color x y img 
    | x < 0 || y < 0 || x > (width img) - 1 || y > (height img) - 1 = img
    | otherwise = floodFillHelper (getColor x y img) color x y img

-- Г

rgbToString :: Rgb -> String
rgbToString rgb = show (red rgb) ++ " " ++ show (green rgb) ++ " " ++ show (blue rgb) ++ "\n"

rgbLineToString :: [Rgb] -> String
rgbLineToString line = foldl (\result rgb -> result ++ (rgbToString rgb)) "" line

rgbContentToString :: [[Rgb]] -> String
rgbContentToString content = foldl (\result line -> result ++ (rgbLineToString line)) "" content

imageToString :: Image -> String
imageToString img = "P3\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n255\n" ++ rgbContentToString (content img)

saveImage :: FilePath -> Image -> IO()
saveImage path img = do
    writeFile path $ imageToString img

-- Д

changeStrings :: [String] -> [Rgb]
changeStrings [] = []
changeStrings (r:g:b:xs) = Rgb (stringToWord r) (stringToWord g) (stringToWord b) : changeStrings xs
    where stringToWord str = fromIntegral (read str :: Word)

convertToContent :: [Rgb] -> Int -> [[Rgb]]
convertToContent [] _ = []
convertToContent lst width = [take width lst] ++ convertToContent (drop width lst) width

getImage :: Int -> Int -> [String] -> IO Image
getImage width height rgbs = do
    if length rgbs /= width * height * 3
        then return emptyImage
        else return (Image width height (convertToContent (changeStrings rgbs) width))

checkThirdLine :: Int -> Int -> [String] -> IO Image
checkThirdLine width height contents = do
    let thirdLine = head contents
    if thirdLine /= "255"
        then return emptyImage
        else getImage width height (words $ unlines $ tail contents)

checkSecondLine :: [String] -> IO Image
checkSecondLine contents = do
    let secondLine = head contents
        dimensions = words secondLine
    if length dimensions /= 2
        then return emptyImage
        else checkThirdLine (read (head dimensions) :: Int) (read (last dimensions) :: Int) (tail contents)

checkFirstLine :: [String] -> IO Image
checkFirstLine contents = do 
    let firstLine = head contents
    if firstLine /= "P3"
        then return emptyImage
        else checkSecondLine $ tail contents

checkNumLines :: [String] -> IO Image
checkNumLines contents = do
    if length contents < 3
        then return emptyImage
        else checkFirstLine contents

loadImage :: String -> IO Image
loadImage path = do
    contents <- readFile path
    checkIfValidFile contents
    where checkIfValidFile contents = checkNumLines $ lines contents
