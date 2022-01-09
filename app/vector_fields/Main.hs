module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import qualified Graphics.Rendering.Cairo as Cairo
import Math.Noise (Perlin(..), perlin, getValue)
import System.Random.MWC

import Draw
import Geometry
import Geometry.Processes.DifferentialEquation
import Graphics.Rendering.Cairo (Render, liftIO)
import Text.Printf (printf)


picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

noiseScale :: Double
noiseScale = 251

-- Randomness seed for reproducible results
seed :: Int
seed = 519496

frames :: [Int]
frames = [0..500]

main :: IO ()
main = do
    gen <- liftIO create
    startPoints <- uniformlyDistributedPoints gen 1000
    thicknesses <- liftIO $ replicateM 1000 (uniformR (0.5, 1.5) gen)
    loop 0 startPoints thicknesses
  where
    loop :: Int -> [Vec2] -> [Double] -> IO ()
    loop frame startPoints thicknesses
        | frame >= 500 = pure ()
        | otherwise = do
            let fileName = printf "out/vector_fields_%05d.png" frame
                t0 = 0.5 * fromIntegral frame
                fieldLines = fieldLine (negateV compositeField) t0 <$> startPoints
            withSurface PNG fileName scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
                Cairo.scale scaleFactor scaleFactor
                restoreStateAfter $ do
                    Cairo.setSourceRGB 1 1 1
                    Cairo.rectangle 0 0 picWidth picHeight
                    Cairo.fill

                for_ (zip fieldLines (cycle thicknesses)) $ \(line, thickness) -> restoreStateAfter $
                    drawFieldLine thickness (take 20 line)
            let newStartPoints = (!! 1) . fieldLine compositeField t0 <$> startPoints
            loop (frame+1) newStartPoints thicknesses

    scaleFactor = 0.5
    scaledWidth = round (picWidth * scaleFactor)
    scaledHeight = round (picHeight * scaleFactor)

uniformlyDistributedPoints :: GenIO -> Int -> IO [Vec2]
uniformlyDistributedPoints gen count = liftIO $ replicateM count randomPoint
  where
    randomPoint = translate correction <$> liftA2 Vec2 (randomCoordinate picWidth) (randomCoordinate picHeight)
    randomCoordinate mx = fmap fromIntegral (uniformR (0, mx) gen :: IO Int)
    correction = Vec2 (-picWidth) 0

drawFieldLine :: Double -> [Vec2] -> Render ()
drawFieldLine _ [] = pure ()
drawFieldLine thickness ps = restoreStateAfter $ do
    let ps' = bezierSmoothen ps
    for_ (zip [1..] ps') $ \(i, p) -> do
        Cairo.setLineWidth (thickness * (50 / (20 + i)))
        bezierSegmentSketch p
        Cairo.stroke

scalarField :: Double -> Vec2 -> Double
scalarField = \t -> noise2d t . scaleXY 1 2
  where
    noise = perlin { perlinFrequency = 1/noiseScale, perlinOctaves = 1, perlinSeed = seed }
    noise2d t (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0.3 * t)

gradientField :: Double -> Vec2 -> Vec2
gradientField t p = noiseScale *. grad (scalarField t) p
  where
    grad f v = 100 *. Vec2 (f (v +. Vec2 0.01 0) - f v) (f (v +. Vec2 0 0.01) - f v)

rotationField :: Double -> Vec2 -> Vec2
rotationField t = rotate (deg 90) . gradientField t

compositeField :: Double -> Vec2 -> Vec2
compositeField t p@(Vec2 x y) = Vec2 1 0 +. 0.8 * perturbationStrength *. rotationField t p
  where
    perturbationStrength = (1 + 0.5 * tanh (4 * (x / picWidth - 0.6))) * exp (-3 * (y / picHeight - 0.5)^2)

fieldLine :: (Double -> Vec2 -> Vec2) -> Double -> Vec2 -> [Vec2]
fieldLine vectorField t0 p0 = [x | (_t, x) <- rungeKuttaConstantStep vectorField p0 t0 dt]
  where
    dt = 10

instance VectorSpace a => VectorSpace (b -> a) where
    (f +. g) a = f a +. g a
    (s *. f) a = s *. f a
    negateV f a = negateV (f a)
