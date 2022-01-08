module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Foldable (for_)
import qualified Graphics.Rendering.Cairo as Cairo
import Numeric.Noise.Perlin
import System.Random.MWC
import Text.Printf

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

frames :: [Double]
frames = [0]

main :: IO ()
main = for_ frames $ \t -> do
    let filename = "out.png" --"out/" ++ show seed ++ "_t" ++ printf "%08.4f" t ++ ".png"
    withSurface PNG filename scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
        Cairo.scale scaleFactor scaleFactor
        restoreStateAfter $ do
            Cairo.setSourceRGB 1 1 1
            Cairo.rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
            Cairo.fill
        don't $ drawScalarField (scalarField t)
        restoreStateAfter $ don't $ do
            Cairo.setSourceRGB 1 0 0
            drawVectorField ((0.2 *.) <$> gradientField t)
        restoreStateAfter $ don't $ do
            Cairo.setSourceRGB 0 0 1
            drawVectorField (compositeField t)
        gen <- liftIO create
        ps <- uniformlyDistributedPoints gen 10000
        thicknesses <- liftIO $ replicateM 1000 (uniformR (0.5, 1.5) gen)
        for_ (zip ps (cycle thicknesses)) $ \(p, thickness) -> restoreStateAfter $ do
            Cairo.setLineWidth thickness
            drawFieldLine (take 20 (fieldLine (compositeField t) p))
  where
    scaleFactor = 4
    scaledWidth = round (fromIntegral picWidth * scaleFactor)
    scaledHeight = round (fromIntegral picHeight * scaleFactor)


uniformlyDistributedPoints :: GenIO -> Int -> Render [Vec2]
uniformlyDistributedPoints gen count = liftIO $ replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate picWidth) (randomCoordinate picHeight)
    randomCoordinate mx = fmap fromIntegral (uniformR (0, mx) gen :: IO Int)

detectClipping :: (Vec2 -> Double) -> Render ()
detectClipping f = restoreStateAfter $
    for_ [ Vec2 (fromIntegral x) (fromIntegral y) | x <- [0,5..picWidth], y <- [0,5..picHeight] ] $ \p@(Vec2 x y) -> do
        let v = f p
            v' | v >= 1 = 1
               | v <= -1 = -1
               | otherwise = 0.5 * v

        hsva 0 0 (0.5 + 0.5 * v') 1
        Cairo.rectangle x y 5 5
        Cairo.fill

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

scalarField :: Double -> Vec2 -> Double
scalarField = \t (Vec2 x y) -> noise2d noise1 (Vec2 (x - t) (2*y))
  where
    noise1 = perlin seed 1 (1/noiseScale) 0.001
    noise2d nf (Vec2 x y) = noiseValue nf (x + 49121616, y + 46216381, 321685163213)

gradientField :: Double -> Vec2 -> Vec2
gradientField t p@(Vec2 x y) =
    let grad f v = 100 *. Vec2 (f (v +. Vec2 0.01 0) - f v) (f (v +. Vec2 0 0.01) - f v)
    in  noiseScale *. grad (scalarField t) p

rotationField :: Double -> Vec2 -> Vec2
rotationField t = rotate (deg 90) . gradientField t

compositeField :: Double -> Vec2 -> Vec2
compositeField t p@(Vec2 x y) = Vec2 1 0 +. 0.8 * perturbationStrength *. rotationField t p
  where
    perturbationStrength = 0.5 * (1 + tanh (4 * (x / fromIntegral picWidth - 0.6))) * exp (-3 * (y / fromIntegral picHeight - 0.5)^2)

fieldLine :: (Vec2 -> Vec2) -> Vec2 -> [Vec2]
fieldLine f p =
    let f' _t y = f y
    in  snd <$> rungeKuttaConstantStep f' p 0 10

don't :: Applicative m => m a -> m ()
don't _ = pure ()
