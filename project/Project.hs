module Project where

import Data.Word

import Image
import Image.PPM
import Image.IO
import Image.IO.PBM
import Image.IO.PGM
import Image.IO.PPM

import Image.Dithering

main :: IO ()
main = do
    contentsFile <- getContentsOfFile
    let contents = (removeComments $ lines contentsFile)
    checkLengthContents contents
    let firstLine = head contents 
    case firstLine of
        "P1" -> do img <- checkFromSecondLine (tail contents) :: IO (Image Bool)
                   algorithm <- getAlgorithmType
                   saveDitheredImage (processImage img algorithm)
        "P2" -> do img <- checkFromSecondLine (tail contents) :: IO (Image Word8)
                   algorithm <- getAlgorithmType
                   saveDitheredImage (processImage img algorithm)
        "P3" -> do img <- checkFromSecondLine (tail contents) :: IO (Image Rgb)
                   algorithm <- getAlgorithmType
                   saveDitheredImage (processImage img algorithm)
        _ -> error "File type must be one of NetPBM's P1, P2 or P3"

-----------------------------------------------------------------

squareImg = Image 100 100 [[ if ((i == 2 || i == 99) && (j /= 1 && j /= 100)) || ((j == 2 || j == 99) && (i /= 1 && i /= 100))
                                then Rgb 0 0 0
                                else Rgb 255 255 255 | i <- [1..100]] | j <- [1..100]]

img1 = Image 3 2 [[Rgb 255 0 0, Rgb 255 128 0, Rgb 255 255 0],
                  [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]

squareImg2 = Image 10 10 [[ if ((i == 2 || i == 9) && (j /= 1 && j /= 10)) || ((j == 2 || j == 9) && (i /= 1 && i /= 10))
                                then Rgb 0 0 0
                                else Rgb 255 255 255 | i <- [1..10]] | j <- [1..10]]