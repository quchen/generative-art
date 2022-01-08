module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Foldable (for_)
import qualified Graphics.Rendering.Cairo as Cairo
import Numeric.Noise.Perlin
import System.Random.MWC

import Draw
import Geometry
import Geometry.Processes.DifferentialEquation
import Graphics.Rendering.Cairo (Render, liftIO)


picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

noiseScale :: Double
noiseScale = 251

seed :: Int
seed = 519496

main :: IO ()
main = withSurface PNG "out.png" scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
    Cairo.scale scaleFactor scaleFactor
    restoreStateAfter $ do
        Cairo.setSourceRGB 1 1 1
        Cairo.rectangle 0 0 picWidth picHeight
        Cairo.fill

    gen <- liftIO create
    ps <- uniformlyDistributedPoints gen 10000
    thicknesses <- liftIO $ replicateM 1000 (uniformR (0.5, 1.5) gen)

    for_ (zip ps (cycle thicknesses)) $ \(p, thickness) -> restoreStateAfter $
        drawFieldLine thickness (take 20 (fieldLine compositeField p))
  where
    scaleFactor = 1
    scaledWidth = round (picWidth * scaleFactor)
    scaledHeight = round (picHeight * scaleFactor)


uniformlyDistributedPoints :: GenIO -> Int -> Render [Vec2]
uniformlyDistributedPoints gen count = liftIO $ replicateM count randomPoint
  where
    randomPoint = translate correction <$> liftA2 Vec2 (randomCoordinate picWidth) (randomCoordinate picHeight)
    randomCoordinate mx = fmap fromIntegral (uniformR (0, mx) gen :: IO Int)
    correction = Vec2 (-0.05 * picWidth) 0

drawFieldLine :: Double -> [Vec2] -> Render ()
drawFieldLine _ [] = pure ()
drawFieldLine thickness ps = restoreStateAfter $ do
    let ps' = bezierSmoothen ps
    for_ (zip [1..] ps') $ \(i, p) -> do
        Cairo.setLineWidth (thickness * (i/10))
        bezierSegmentSketch p
        Cairo.stroke

scalarField :: Vec2 -> Double
scalarField = \(Vec2 x y) -> noise2d noise1 (Vec2 x (2*y))
  where
    noise1 = perlin seed 1 (1/noiseScale) 0.001
    noise2d nf (Vec2 x y) = noiseValue nf (x + 49121616, y + 46216381, 321685163213)

gradientField :: Vec2 -> Vec2
gradientField p = noiseScale *. grad scalarField p
  where
    grad f v = 100 *. Vec2 (f (v +. Vec2 0.01 0) - f v) (f (v +. Vec2 0 0.01) - f v)

rotationField :: Vec2 -> Vec2
rotationField = rotate (deg 90) . gradientField

compositeField :: Vec2 -> Vec2
compositeField p@(Vec2 x y) = Vec2 1 0 +. 0.8 * perturbationStrength *. rotationField p
  where
    perturbationStrength = 0.5 * (1 + tanh (4 * (x / picWidth - 0.6))) * exp (-3 * (y / picHeight - 0.5)^2)

fieldLine :: (Vec2 -> Vec2) -> Vec2 -> [Vec2]
fieldLine f p = snd <$> rungeKuttaConstantStep (const f) p 0 10
