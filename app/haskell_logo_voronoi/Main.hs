{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative             (liftA2)
import           Control.Monad                   (replicateM)
import           Data.Char                       (ord)
import           Data.Foldable                   (for_)
import           Data.List                       (find)
import           Data.Maybe                      (mapMaybe)
import           Data.Vector                     (fromList)
import           Prelude                         hiding ((**))
import           System.Environment              (getArgs)
import           System.FilePath                 (splitExtension)
import           System.Random.MWC               (GenIO, initialize, uniformR)
import           System.Random.MWC.Distributions (normal)

import           Geometry
import           Graphics.Rendering.Cairo        hiding (transform)
import           Voronoi
import           Draw

data RGB = RGB { r :: Double, g :: Double, b :: Double }

data OutputFormat = PNG | SVG

fromExtension :: String -> OutputFormat
fromExtension ".png" = PNG
fromExtension ".svg" = SVG
fromExtension other  = error ("Unknown file format: " <> other)

main :: IO ()
main = mainHaskellLogo

mainHaskellLogo :: IO ()
mainHaskellLogo = do
    [count, file] <- getArgs
    let (_, extension) = splitExtension file
    let w, h :: Num a => a
        w = 1200
        h = 1200
        haskellLogoCentered = fmap (fmap (transform (translate' (w/2 + 30) (h/2)) . transform (scale' 2 2))) haskellLogo
    gen <- initialize (fromList (map (fromIntegral . ord) count))
    points <- gaussianDistributedPoints gen (w, 380) (h, 380) (read count)
    let picturePoints = mapMaybe (\point -> fmap (\(color, _) -> (point, color)) $ find (pointInPolygon point . snd) haskellLogoCentered) points
        backgroundPoints = fmap (\point -> (point, darkGrey)) $ filter (\point -> not $ any (pointInPolygon point . snd) haskellLogoCentered) points
        allPoints = picturePoints ++ backgroundPoints
        allFaces = faces (voronoi allPoints w h)
    withSurface (fromExtension extension) file w h $ \surface -> renderWith surface $ for_ allFaces drawFace

withSurface :: OutputFormat -> FilePath -> Int -> Int -> (Surface -> IO a) -> IO a
withSurface PNG = withPNGSurface
withSurface SVG = \f w h -> withSVGSurface f (fromIntegral w) (fromIntegral h)


withPNGSurface :: FilePath -> Int -> Int -> (Surface -> IO a) -> IO a
withPNGSurface file w h action = do
    surface <- createImageSurface FormatARGB32 w h
    result <- action surface
    surfaceWriteToPNG surface file
    pure result

drawFace :: VoronoiFace RGB -> Render ()
drawFace VF{..} = drawPoly face props

drawPoly :: Polygon -> RGB -> Render ()
drawPoly (Polygon []) _ = pure ()
drawPoly poly color = do
    let fillColor = color
        lineColor = lighten 0.2 color
    polygonSketch poly
    setColor fillColor
    fillPreserve
    setColor lineColor
    setLineWidth 1
    stroke

setColor :: RGB -> Render ()
setColor RGB{..} = setSourceRGB r g b

uniformlyDistributedPoints :: GenIO -> Int -> Int -> Int -> IO [Vec2]
uniformlyDistributedPoints gen width height count = replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width) (randomCoordinate height)
    randomCoordinate mx = fmap fromIntegral (uniformR (0, mx) gen :: IO Int)

gaussianDistributedPoints :: GenIO -> (Int, Double) -> (Int, Double) -> Int -> IO [Vec2]
gaussianDistributedPoints gen (width, sigmaX) (height, sigmaY) count = replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width sigmaX) (randomCoordinate height sigmaY)
    randomCoordinate mx sigma = do
        coord <- normal (fromIntegral mx/2) sigma gen :: IO Double
        if coord < 0 || coord > fromIntegral mx
            then randomCoordinate mx sigma
            else pure coord

-- 480 x 340, centered around (0, 0)
haskellLogo :: [(RGB, Polygon)]
haskellLogo = zip colors $ fmap (transform (translate' (-240) (-170))) [left, lambda, upper, lower]
  where
    left   = Polygon [Vec2 0 340.15625, Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625, Vec2 0 340.15625]
    lambda = Polygon [Vec2 113.386719 340.15625, Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625, Vec2 113.386719 340.15625]
    upper  = Polygon [Vec2 387.402344 240.945312, Vec2 349.609375 184.253906, Vec2 481.890625 184.25, Vec2 481.890625 240.945312, Vec2 387.402344 240.945312]
    lower = Polygon [Vec2 330.710938 155.90625, Vec2 292.914062 99.214844, Vec2 481.890625 99.210938, Vec2 481.890625 155.90625, Vec2 330.710938 155.90625]

    colors = fmap parseHex [ "453a62", "5e5086", "8f4e8b", "8f4e8b" ]

parseHex :: String -> RGB
parseHex [r1, r2, g1, g2, b1, b2] = RGB
    { r = read ("0x" ++ [r1, r2]) / 255
    , g = read ("0x" ++ [g1, g2]) / 255
    , b = read ("0x" ++ [b1, b2]) / 255 }

darkGrey :: RGB
darkGrey = RGB 0.1 0.1 0.1

lighten :: Double -> RGB -> RGB
lighten d RGB{..} = RGB (r + d) (g + d) (b + d)
