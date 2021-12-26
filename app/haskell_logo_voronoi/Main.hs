{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Applicative             (liftA2)
import           Control.Monad                   (replicateM)
import           Data.Char                       (ord)
import           Data.Foldable                   (for_)
import           Data.List                       (find)
import           Data.Maybe                      (mapMaybe)
import           Data.Vector                     (fromList)
import           Prelude                         hiding ((**))
import           System.Environment              (getArgs)
import           System.Random.MWC               (GenIO, initialize, uniformR)
import           System.Random.MWC.Distributions (normal)

import           Draw
import           Geometry
import           Geometry.Shapes                 (haskellLogo)
import           Graphics.Rendering.Cairo        hiding (transform)
import           Voronoi

data RGB = RGB { r :: Double, g :: Double, b :: Double }

main :: IO ()
main = mainHaskellLogo

mainHaskellLogo :: IO ()
mainHaskellLogo = do
    let count = "2048"
    let w, h :: Num a => a
        w = 1200
        h = 1200
        haskellLogoCentered = transform (translate' (Vec2 (w/2 - 480) (h/2 - 340)) <> scale' 680 680) $ haskellLogo
    gen <- initialize (fromList (map (fromIntegral . ord) count))
    points <- gaussianDistributedPoints gen (w, 380) (h, 380) (read count)
    let picturePoints = mapMaybe (\point -> fmap (\(color, _) -> (point, color)) $ find (pointInPolygon point . snd) (zip haskellLogoColors haskellLogoCentered)) points
        backgroundPoints = fmap (\point -> (point, darkGrey)) $ filter (\point -> not $ any (pointInPolygon point) haskellLogoCentered) points
        allPoints = picturePoints ++ backgroundPoints
        allFaces = faces (mkVoronoi w h allPoints)
    withSurfaceAuto "out/haskell_logo_voronoi.png" w h $ \surface -> renderWith surface $ for_ allFaces drawFace
    withSurfaceAuto "out/haskell_logo_voronoi.svg" w h $ \surface -> renderWith surface $ for_ allFaces drawFace

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


haskellLogoColors :: [RGB]
haskellLogoColors = fmap parseHex [ "453a62", "5e5086", "8f4e8b", "8f4e8b" ]

parseHex :: String -> RGB
parseHex [r1, r2, g1, g2, b1, b2] = RGB
    { r = read ("0x" ++ [r1, r2]) / 255
    , g = read ("0x" ++ [g1, g2]) / 255
    , b = read ("0x" ++ [b1, b2]) / 255 }

darkGrey :: RGB
darkGrey = RGB 0.1 0.1 0.1

lighten :: Double -> RGB -> RGB
lighten d RGB{..} = RGB (r + d) (g + d) (b + d)
