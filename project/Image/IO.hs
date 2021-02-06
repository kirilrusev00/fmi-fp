module Image.IO where

import Image
import Data.Word
import Control.Exception

getInputImagePath :: IO String
getInputImagePath = do
    putStrLn "Enter the path of the input image:"
    getLine

getContentsOfFile :: IO String
getContentsOfFile = do
    filePath <- getInputImagePath
    contentsText <- try $ readFile filePath :: IO (Either SomeException String)
    case contentsText of
        Left exception -> do
            putStrLn ("Could not find file " ++ filePath ++ ".")
            print exception
            putStrLn "Try again."
            getContentsOfFile
        Right contents -> return contents

removeComments :: [String] -> [String]
removeComments [] = []
removeComments (x:xs) 
    | removeCommentsFromLine x == "" = removeComments xs
    | otherwise = (removeCommentsFromLine x) : removeComments xs
    where removeCommentsFromLine x = if null occurencesOf then x else take (head occurencesOf) x
          occurencesOf = [ n | (m, n) <- zip x [0..], m == '#' ]

checkLengthContents :: [String] -> IO ()
checkLengthContents contents = do
    if length contents < 3
        then error "File must have at least 3 lines"
        else return () 

class InputImage a where
    checkFromSecondLine :: [String] -> IO (Image a)

    checkFromThirdLine :: Int -> Int -> [String] -> IO (Image a)

    getImage :: Int -> Int -> [String] -> IO (Image a)

    convertToContent :: [a] -> Int -> [[a]]
    convertToContent [] _ = []
    convertToContent lst width = [take width lst] ++ convertToContent (drop width lst) width

    parseStrings :: [String] -> [a]

parseStringToColor :: String -> Word8
parseStringToColor string
    | stringAsWord < 0 || stringAsWord > 255 = error "Color values must be from 0 to 255"
    | otherwise = fromIntegral stringAsWord
    where stringAsWord = (read string :: Word)

inputAlgorithm :: IO String
inputAlgorithm = do
    putStrLn ("Choose a dithering algorithm. Type:\n" ++
             "\t\"1D\" for one-dimensional dithering\n" ++
             "\t\"FS\" for Floyd-Steinberg dithering\n" ++
             "\t\"B4x4\" for ordered dithering using a 4x4 Bayer matrix\n" ++
             "\t\"B8x8\" for ordered dithering using a 8x8 Bayer matrix\n" ++
             "\t\"Burkes\" for Burkes dithering\n" ++
             "\t\"TRS\" for Two-Row Sierra dithering\n" ++
             "\t\"SL\" for Sierra Lite dithering\n" ++
             "\t\"JJN\" for Jarvice, Judice and Ninke dithering\n" ++
             "\t\"Stucki\" for Stucki dithering\n" ++
             "\t\"Atkinson\" for Atkinson dithering\n" ++
             "\t\"Sierra\" for Sierra dithering") 
    getLine

getAlgorithmType :: IO String
getAlgorithmType = do
    alg <- inputAlgorithm
    if (alg /= "1D" && alg /= "FS" && alg /= "B4x4" && alg /= "B8x8" && alg /= "Burkes" && alg /= "TRS"
         && alg /= "SL" && alg /= "JJN" && alg /= "Stucki" && alg /= "Atkinson" && alg /= "Sierra")
         then do
             putStrLn "Algorithm not recognised. Try again."
             getAlgorithmType
         else return alg

--------------------------------------------------------------------------------

class OutputImage a where
    pixelToString :: a -> String

    lineToString :: [a] -> String
    lineToString line = foldl (\result pixel -> result ++ (pixelToString pixel)) "" line

    contentToString :: [[a]] -> String
    contentToString content = foldl (\result line -> result ++ (lineToString line)) "" content

    imageToString :: Image a -> String

    saveImage :: FilePath -> Image a -> IO ()
    saveImage path img = do
        contentsText <- try (writeFile path $ imageToString img) :: IO (Either SomeException ()) 
        case contentsText of
            Left exception -> do
                putStrLn ("An error occured while writing to file " ++ path ++ ".")
                print exception
                saveDitheredImage img
            Right _ -> putStrLn "Image successfully saved."

    saveDitheredImage :: Image a -> IO ()
    saveDitheredImage img = do
        filePath <- getOutputImagePath
        case filePath of
            "cancel" -> putStrLn "Operation canceled."
            _ -> saveImage filePath img

getOutputImagePath :: IO String
getOutputImagePath = do
    putStrLn "Enter the path of the output image or \"cancel\" to cancel the operation:"
    getLine