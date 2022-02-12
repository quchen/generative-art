module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import Math.Noise (Perlin (..), perlin, getValue)

import Geometry.Algorithms.Path.Dijkstra
import Geometry.Algorithms.Path.Optimize

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
            , _maxIndex = (round (picWidth / cellSize + 4), round (picHeight / cellSize + 4))
            }
        isoLine = isoLines grid scalarField

    let costFunction p = exp (8 * scalarField p)
        width = picWidth
        height = picHeight
        step = 20
        trajectory start end = optimizePath 10000 0.1 costFunction $ simplifyTrajectoryRadial 100 $ dijkstra Dijkstra {..} start end

        trajectories =
            [ trajectory start end
            | (start, end) <-
                [ (Vec2 1400 1440, Vec2 100 0)
                , (Vec2 1200 1440, Vec2 0 1200)
                , (Vec2 1800 1440, Vec2 900 0)
                , (Vec2 600 1440, Vec2 900 0)
                ]
            ]


    for_ [-0.6,-0.55..0.1] $ \v ->
        drawAtHeight v $ drawIsoLine v (isoLine v)

    drawAtHeight 0 $ for_ trajectories $ \path -> do
        bezierCurveSketch $ bezierSmoothenOpen path
        Cairo.setLineWidth 5
        setColor (white `withOpacity` 0.6)
        Cairo.stroke

    for_ [0.1,0.15..0.5] $ \v ->
        drawAtHeight v $ drawIsoLine v (isoLine v)

  where
    scaleFactor = 0.39
    scaledWidth = round (picWidth * scaleFactor)
    scaledHeight = round (picHeight * scaleFactor)

everyNth :: Int -> [a] -> [a]
everyNth _ [] = []
everyNth n (x:xs) = x : everyNth n (drop (n-1) xs)

drawAtHeight :: Double -> Cairo.Render () -> Cairo.Render ()
drawAtHeight v action = cairoScope $ do
    Cairo.translate 0 (-300*v)
    Cairo.translate (picWidth/2) (picHeight/2)
    Cairo.scale 2 0.5
    Cairo.rotate (0.25*pi)
    Cairo.translate (-picWidth/2) (-picHeight/2)
    Cairo.rectangle (0.5 * (picWidth - picHeight)) 0 picHeight picHeight
    Cairo.clip
    action

drawIsoLine :: Double -> [[Vec2]] -> Cairo.Render ()
drawIsoLine v ls = do
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
    | x < -0 || x > picWidth || y < 0 || y > picHeight = -1/0
    | otherwise = noise2d
  where
    noise = perlin { perlinFrequency = 1/noiseScale, perlinOctaves = 1, perlinSeed = seed }
    noise2d = fromMaybe 0 $ getValue noise (x, y, 0)
