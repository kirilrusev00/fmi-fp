module Image.IO.PPM where

import Image
import Data.Word
import Image.PPM
import Image.IO

instance InputImage Rgb where
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

    getImage width height rgbs = do
        if length rgbs /= width * height * 3
            then error "Image pixels information is not ok"
            else return (Image width height (convertToContent (parseStrings rgbs) width))

    parseStrings [] = []
    parseStrings (r:g:b:xs) = Rgb (parseStringToColor r) (parseStringToColor g) (parseStringToColor b) : parseStrings xs

--------------------------------------------------------------------------------

instance OutputImage Rgb where
    pixelToString rgb = show (red rgb) ++ " " ++ show (green rgb) ++ " " ++ show (blue rgb) ++ "\n"

    imageToString img = "P3\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n255\n" 
                               ++ contentToString (content img)