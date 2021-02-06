module Image.Dithering where

import Image
import Image.Dithering.ErrorDiffusion.Atkinson
import Image.Dithering.ErrorDiffusion.Burkes
import Image.Dithering.ErrorDiffusion.FloydSteinberg
import Image.Dithering.ErrorDiffusion.JarvisJudiceNinke
import Image.Dithering.ErrorDiffusion.OneDimensional
import Image.Dithering.ErrorDiffusion.Sierra
import Image.Dithering.ErrorDiffusion.SierraLite
import Image.Dithering.ErrorDiffusion.Stucki
import Image.Dithering.ErrorDiffusion.TwoRowSierra
import Image.Dithering.Ordered

processImage :: (OneDimensionalDither a, FloydSteinbergDither a, BurkesDither a, TwoRowSierraDither a, SierraLiteDither a, 
                    OrderedDitherImage a, JarvisJudiceNinkeDither a, StuckiDither a, AtkinsonDither a, SierraDither a)
                                    => Image a -> String -> Image a
processImage img algorithm = case algorithm of
                                    "1D" -> ditherOneDimensional img
                                    "FS" -> ditherFS img
                                    "B4x4" -> ditherBayer4X4 img
                                    "B8x8" -> ditherBayer8X8 img
                                    "Burkes" -> ditherBurkes img
                                    "TRS" -> ditherTwoRowSierra img
                                    "SL" -> ditherSierraLite img
                                    "JJN" -> ditherJarvisJudiceNinke img
                                    "Stucki" -> ditherStucki img
                                    "Atkinson" -> ditherAtkinson img
                                    "Sierra" -> ditherSierra img
                                    _ -> error "Algorithm not recognised"