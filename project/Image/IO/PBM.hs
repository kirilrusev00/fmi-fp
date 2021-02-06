module Image.IO.PBM where

import Image
import Data.Word
import Image.IO

instance InputImage Bool where
    checkFromSecondLine contents = do
        let secondLine = head contents
            dimensions = words secondLine
        if length dimensions /= 2
            then error "Second line must contain the width and the height of the image"
            else getImage (read (head dimensions) :: Int) (read (last dimensions) :: Int) (words $ unlines $ tail contents)

    checkFromThirdLine width height contents = undefined

    getImage width height colors = do
        if length colors /= width * height
            then error "Image pixels information is not ok"
            else return (Image width height (convertToContent (parseStrings colors) width))

    parseStrings [] = []
    parseStrings (x:xs) = (parseStringToBW x) : parseStrings xs

parseStringToBW :: String -> Bool
parseStringToBW string
    | stringAsWord == 0 = False 
    | stringAsWord == 1 = True
    | otherwise = error "Color values must be either 0 or 1"
    where stringAsWord = (read string :: Word)

--------------------------------------------------------------------------------

instance OutputImage Bool where
    pixelToString pixel = show (fromEnum pixel) ++ "\n"

    imageToString img = "P1\n" ++ show (width img) ++ " " ++ show (height img)
                               ++ contentToString (content img)
