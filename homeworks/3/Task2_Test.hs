import Task2
import Test.HUnit

img1 :: Image
img1 = Image 3 2 [[Rgb 255 0 0, Rgb 255 128 0, Rgb 255 255 0],
                  [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]]

grayscaleImg1 :: Image
grayscaleImg1 = Image 3 2 [[Rgb 76 76 76, Rgb 152 152 152, Rgb 226 226 226],
                           [Rgb 150 150 150, Rgb 255 255 255, Rgb 202 202 202]]

testListGrayscale = TestList [

    "Grayscale image of empty image is empty image" ~:
        (grayscale emptyImage) ~?= emptyImage,

    "Grayscale image of img1 is calculated correctly" ~:
        (grayscale img1) ~?= grayscaleImg1

    ]

squareImg :: Image
squareImg = Image 100 100 [[ if ((i == 2 || i == 99) && (j /= 1 && j /= 100)) || ((j == 2 || j == 99) && (i /= 1 && i /= 100))
                                then Rgb 0 0 0
                                else Rgb 255 255 255 | i <- [1..100]] | j <- [1..100]]

squareImgED :: Image
squareImgED = Image 100 100 [[ if i == 1 || i == 3 || i == 98 || i == 100 || j == 1 || j == 3 || j == 98 || j == 100
                               || (i == 2 && j == 2) || (i == 2 && j == 99) || (i == 99 && j == 2) || (i == 99 && j == 99)
                                then Rgb 255 255 255
                                else Rgb 0 0 0 | i <- [1..100]] | j <- [1..100]]

testListEdgeDetect = TestList [

    "Edge detect image of empty image is empty image" ~:
        (edgeDetect emptyImage) ~?= emptyImage,

    "Edge detect image of squareImg is calculated correctly" ~:
        (edgeDetect squareImg) ~?= squareImgED,

    "Returns the same image if it is not grayscale" ~:
        (edgeDetect img1) ~?= img1
    ]

smallSquareImg :: Image
smallSquareImg = Image 10 10 [[ if ((i == 2 || i == 9) && (j /= 1 && j /= 10)) || ((j == 2 || j == 9) && (i /= 1 && i /= 10))
                                then Rgb 0 0 0
                                else Rgb 255 255 255 | i <- [1..10]] | j <- [1..10]]

smallSquareFill_0_0 :: Image
smallSquareFill_0_0 = Image 10 10 [[ if ((i == 2 || i == 9) && (j /= 1 && j /= 10)) || ((j == 2 || j == 9) && (i /= 1 && i /= 10))
                                        then Rgb 0 0 0
                                        else if i == 1 || i == 10 || j == 1 || j == 10 
                                            then Rgb 255 0 0
                                            else Rgb 255 255 255 | i <- [1..10]] | j <- [1..10]]
smallSquareFill_1_1 :: Image
smallSquareFill_1_1 = Image 10 10 [[ Rgb 255 255 255 | i <- [1..10]] | j <- [1..10]]

smallSquareFill_5_5 :: Image
smallSquareFill_5_5 = Image 10 10 [[ if ((i == 2 || i == 9) && (j /= 1 && j /= 10)) || ((j == 2 || j == 9) && (i /= 1 && i /= 10))
                                        then Rgb 0 0 0
                                        else if i == 1 || i == 10 || j == 1 || j == 10 
                                            then Rgb 255 255 255
                                            else Rgb 0 255 0 | i <- [1..10]] | j <- [1..10]]

testListFloodFill = TestList [

    "Flood fill performed on empty image returns empty image" ~:
        (floodFill (Rgb 0 0 0) 0 0 emptyImage) ~?= emptyImage,

    "Flood fill performed on border of an image with square on it fills only the border" ~:
        (floodFill (Rgb 255 0 0) 0 0 smallSquareImg) ~?= smallSquareFill_0_0,

    "Flood fill performed on the square in an image with square on it with the color of the background returns image painted in this color" ~:
        (floodFill (Rgb 255 255 255) 1 1 smallSquareImg) ~?= smallSquareFill_1_1,

    "Flood fill performed on the inner part of the square in an image with square on it fills only the inner part of the square" ~:
        (floodFill (Rgb 0 255 0) 5 5 smallSquareImg) ~?= smallSquareFill_5_5,

    "Flood fill with coordinates bigger than the sizes of the image returns the same image" ~:
        (floodFill (Rgb 0 255 0) 11 11 smallSquareImg) ~?= smallSquareImg,

    "Flood fill with coordinates smaller than 0 returns the same image" ~:
        (floodFill (Rgb 0 255 0) (-1) (-1) smallSquareImg) ~?= smallSquareImg

    ]

-- Function to test with real images (supposed the .ppm images are in directory ./images)
testImage :: String -> IO()
testImage fileName = do
    image <- loadImage ("images/" ++ fileName ++ ".ppm")
    putStrLn "Successfully loaded original image"
    saveImage ("images/" ++ fileName ++ "Gr.ppm") (grayscale image)
    putStrLn "Successfully saved grayscale image"
    gr <- loadImage ("images/" ++ fileName ++ "Gr.ppm")
    putStrLn "Successfully loaded grayscale image"
    saveImage ("images/" ++ fileName ++ "ED.ppm") (edgeDetect gr)
    putStrLn "Successfully loaded edgeDetect image"

main = do
    runTestTT testListGrayscale
    runTestTT testListEdgeDetect
    runTestTT testListFloodFill
