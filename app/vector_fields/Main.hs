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


picWidth, picHeight :: Int
picWidth = 2560
picHeight = 1440

noiseScale :: Double
noiseScale = 251

seed :: Int
seed = 519496

main :: IO ()
main = withSurface PNG "out.png" (picWidth * scaleFactor) (picHeight * scaleFactor) $ \surface -> Cairo.renderWith surface $ do
    Cairo.scale scaleFactor scaleFactor
    restoreStateAfter $ do
        Cairo.setSourceRGB 1 1 1
        Cairo.rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
        Cairo.fill
    don't $ drawScalarField scalarField
    restoreStateAfter $ don't $ do
        Cairo.setSourceRGB 1 0 0
        drawVectorField ((0.2 *.) <$> gradientField)
    restoreStateAfter $ don't $ do
        Cairo.setSourceRGB 0 0 1
        drawVectorField compositeField
    gen <- liftIO create
    ps <- uniformlyDistributedPoints gen 50000
    thicknesses <- liftIO $ replicateM 1000 (uniformR (0.2, 0.7) gen)
    for_ (zip ps (cycle thicknesses)) $ \(p, thickness) -> restoreStateAfter $ do
        Cairo.setLineWidth thickness
        drawFieldLine (take 10 (fieldLine compositeField p))
  where
    scaleFactor :: Num a => a
    scaleFactor = 2


uniformlyDistributedPoints :: GenIO -> Int -> Render [Vec2]
uniformlyDistributedPoints gen count = liftIO $ replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate picWidth) (randomCoordinate picHeight)
    randomCoordinate mx = fmap fromIntegral (uniformR (0, mx) gen :: IO Int)

drawScalarField :: (Vec2 -> Double) -> Render ()
drawScalarField f = restoreStateAfter $
    for_ [ Vec2 (fromIntegral x) (fromIntegral y) | x <- [0..picWidth], y <- [0..picHeight] ] $ \p@(Vec2 x y) -> do
        hsva 0 0 (0.5 + 0.25 * f p) 1
        Cairo.rectangle x y 1 1
        Cairo.fill

drawVectorField :: (Vec2 -> Vec2) -> Render ()
drawVectorField f = restoreStateAfter $
    for_ [0,20..picWidth] $ \x -> for_ [0,20..picHeight] $ \y -> do
        let point = Vec2 (fromIntegral x) (fromIntegral y)
            end = point +. 20 *. f point
        arrowSketch (Line point end) def { arrowheadSize = Distance 5 }
        Cairo.stroke

drawFieldLine :: [Vec2] -> Render ()
drawFieldLine [] = pure ()
drawFieldLine ps = restoreStateAfter $ do
    bezierCurveSketch (bezierSmoothenOpen ps)
    Cairo.stroke

scalarField :: Vec2 -> Double
scalarField (Vec2 x y) = 0.8 * noise2d noise1 (Vec2 x (2*y)) + 0.2 * noise2d noise2 (Vec2 x (2*y))
  where
    -- Average over two noise functions b/c perlin from hsnoise seems to lead to some clipping
    noise1 = perlin seed 3 (1/noiseScale) 0.2
    noise2 = perlin (seed+1) 3 (1/noiseScale) 0.2
    noise2d nf (Vec2 x y) = noiseValue nf (x + 49156616, y + 46216981, 321685163213)

gradientField :: Vec2 -> Vec2
gradientField p@(Vec2 x y) =
    let grad f v = 100 *. Vec2 (f (v +. Vec2 0.01 0) - f v) (f (v +. Vec2 0 0.01) - f v)
    in  noiseScale *. grad scalarField p

rotationField :: Vec2 -> Vec2
rotationField = rotate (deg 90) . gradientField

compositeField :: Vec2 -> Vec2
compositeField p@(Vec2 x y) = Vec2 1 0 +. 0.6 * perturbationStrength *. rotationField p
  where
    perturbationStrength = 0.5 * (1 + tanh (4 * (x / fromIntegral picWidth - 0.6))) * exp (-3 * (y / fromIntegral picHeight - 0.5)^2)

fieldLine :: (Vec2 -> Vec2) -> Vec2 -> [Vec2]
fieldLine f p =
    let f' _t y = f y
    in  snd <$> rungeKuttaConstantStep f' p 0 10

don't :: Applicative m => m a -> m ()
don't _ = pure ()
