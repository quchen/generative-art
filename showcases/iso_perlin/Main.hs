module Main where

import Data.Maybe (fromMaybe)
import qualified Graphics.Rendering.Cairo as Cairo
import Math.Noise (Perlin (..), perlin, getValue)

import Draw
import Geometry



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

noiseScale :: Double
noiseScale = 251

-- Randomness seed for reproducible results
seed :: Int
seed = 519496

main :: IO ()
main = withSurfaceAuto "out/iso-perlin.png" scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
    Cairo.scale scaleFactor scaleFactor
    cairoScope $ do
        Cairo.setSourceRGB 1 1 1
        Cairo.paint

    let grid = Grid { _range = (Vec2 0 0, Vec2 picWidth picHeight), _numCells = (picWidth `div` 10, picHeight `div` 10)}
        isoLine = isoLines grid scalarField

    for_ [-1,-0.9..1] $ \v ->
        drawIsoLine (isoLine v)
  where
    scaleFactor = 0.39
    scaledWidth = round (picWidth * scaleFactor)
    scaledHeight = round (picHeight * scaleFactor)

drawIsoLine :: [[Vec2]] -> Cairo.Render ()
drawIsoLine pss = for_ pss $ \(p:ps) -> do
    moveToVec p
    for_ ps lineToVec
    Cairo.stroke

scalarField :: Vec2 -> Double
scalarField = noise2d . transform (scale' 1 2)
  where
    noise = perlin { perlinFrequency = 1/noiseScale, perlinOctaves = 1, perlinSeed = seed }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)
