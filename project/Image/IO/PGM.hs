module Image.IO.PGM where

import Image
import Data.Word
import Image.IO

instance InputImage Word8 where
    checkFromSecondLine contents = do
        let secondLine = head contents
            dimensions = words secondLine
        if length dimensions /= 2
            then error "Second line must contain the width and the height of the image"
            else checkFromThirdLine (read (head dimensions) :: Int) (read (last dimensions) :: Int) (tail contents)

    checkFromThirdLine width height contents = do
        let thirdLine = head contents
        if thirdLine /= "255"
            then error "Only images with color values from 0 to 255 are supported"
            else getImage width height (words $ unlines $ tail contents)

    getImage width height colors = do
        if length colors /= width * height
            then error "Image pixels information is not ok"
            else return (Image width height (convertToContent (parseStrings colors) width))

    parseStrings [] = []
    parseStrings (x:xs) = (parseStringToColor x) : parseStrings xs

--------------------------------------------------------------------------------

instance OutputImage Word8 where
    pixelToString pixel = show pixel ++ "\n"

    imageToString img = "P2\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n255\n" 
                               ++ contentToString (content img)