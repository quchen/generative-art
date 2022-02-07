module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import Math.Noise (Perlin (..), perlin, getValue)

import Draw
import Geometry



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
            { _range = (Vec2 (-2*cellSize) (-2*cellSize), Vec2 (picWidth + 2*cellSize) (picHeight + 2*cellSize))
            , _numCells = (round (picWidth / cellSize + 4), round (picHeight / cellSize + 4))
            }
        isoLine = isoLines grid scalarField

    for_ [-0.5,-0.45..0.5] $ \v ->
        drawIsoLine v (isoLine v)
  where
    scaleFactor = 0.39
    scaledWidth = round (picWidth * scaleFactor)
    scaledHeight = round (picHeight * scaleFactor)

drawIsoLine :: Double -> [[Vec2]] -> Cairo.Render ()
drawIsoLine v ls = grouped Cairo.paint $ do
    Cairo.translate 0 (-200*v)
    Cairo.translate (picWidth/2) (picHeight/2)
    Cairo.scale 2 0.5
    Cairo.rotate (0.25*pi)
    Cairo.translate (-picWidth/2) (-picHeight/2)
    Cairo.rectangle (0.5 * (picWidth - picHeight)) 0 picHeight picHeight
    Cairo.clip
    grouped Cairo.paint $ for_ ls $ \l -> cairoScope $ do
        smoothLineSketch l
        Cairo.setOperator Cairo.OperatorXor
        setColor (inferno (v+0.45))
        Cairo.fill
    for_ ls $ \l -> cairoScope $ do
        smoothLineSketch l
        Cairo.setLineWidth 5
        setColor (inferno (v+0.5))
        Cairo.stroke
  where
    smoothLineSketch l = bezierCurveSketch (V.toList (bezierSmoothenLoop (V.fromList l))) >> Cairo.closePath

scalarField :: Vec2 -> Double
scalarField (Vec2 x y)
    | x < 0 || x > picWidth || y < 0 || y > picHeight = -1/0
    | otherwise = noise2d
  where
    noise = perlin { perlinFrequency = 1/noiseScale, perlinOctaves = 1, perlinSeed = seed }
    noise2d = fromMaybe 0 $ getValue noise (x, y, 0)
