module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import Math.Noise (Perlin (..), perlin, getValue)

import Draw
import Geometry
import Debug.Trace



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

noiseScale :: Double
noiseScale = 250

-- Randomness seed for reproducible results
seed :: Int
seed = 511151

main :: IO ()
main = withSurfaceAuto "out/iso-perlin.png" scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ do
        Cairo.setSourceRGB 1 1 1
        Cairo.paint

    let cellSize = 10
        grid = Grid
            { _range = (Vec2 (-cellSize) (-cellSize), Vec2 (picWidth + cellSize) (picHeight + cellSize))
            , _maxIndex = (round (picWidth / cellSize + 2), round (picHeight / cellSize + 2))
            }
        isoLine = isoLines grid scalarField

    for_ [-0.5,-0.4..0.5] $ \v ->
        drawIsoLine v (isoLine v)
  where
    scaleFactor = 0.39
    scaledWidth = round (picWidth * scaleFactor)
    scaledHeight = round (picHeight * scaleFactor)

drawIsoLine :: Double -> [[Vec2]] -> Cairo.Render ()
drawIsoLine v ls = for_ ls $ \l -> do
    let smoothLine = V.toList $ bezierSmoothenLoop (V.fromList $ traceShowId l)
    bezierCurveSketch smoothLine
    Cairo.closePath
    setColor (inferno (v+0.5))
    Cairo.fill

scalarField :: Vec2 -> Double
scalarField (Vec2 x y)
    | x < 0 || x > picWidth || y < 0 || y > picHeight = -1/0
    | otherwise = noise2d
  where
    noise = perlin { perlinFrequency = 1/noiseScale, perlinOctaves = 1, perlinSeed = seed }
    noise2d = fromMaybe 0 $ getValue noise (x, y, 0)
