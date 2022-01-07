module Main where

import Data.Foldable (for_)
import Graphics.Rendering.Cairo as Cairo hiding (x, y)
import Numeric.Noise.Perlin

import Draw
import Geometry


picWidth, picHeight :: Int
picWidth = 2560
picHeight = 1440

noiseScale :: Double
noiseScale = 100

seed :: Int
seed = 123456

main :: IO ()
main = withSurface PNG "out.png" picWidth picHeight $ \surface -> renderWith surface $ do
    restoreStateAfter $ do
        Cairo.setSourceRGB 1 1 1
        Cairo.rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
        Cairo.fill
    let perturbation = perlin seed 1 (1/noiseScale) 0.5
    --drawVectorField (gradientField3 perturbation)
    drawFieldLines (gradientField3 perturbation) (Vec2 0 . fromIntegral <$> [0,10..picHeight])

drawVectorField :: (Vec2 -> Vec2) -> Render ()
drawVectorField f = restoreStateAfter $ do
    setSourceRGB 0 0 0
    for_ [0,40..picWidth] $ \x -> for_ [0,40..picHeight] $ \y -> do
        let point = Vec2 (fromIntegral x) (fromIntegral y)
            end = point +. 20 *. f point
        arrowSketch (Line point end) def
        stroke

drawFieldLines :: (Vec2 -> Vec2) -> [Vec2] -> Render ()
drawFieldLines f ps = restoreStateAfter $ do
    setSourceRGB 0 0 0
    for_ ps $ \p -> do
        moveToVec p
        for_ (gradientDescent f p) lineToVec
        Cairo.stroke

gradientField :: Perlin -> Vec2 -> Vec2
gradientField perturbation p@(Vec2 x y) =
    let perturbationStrength = x / fromIntegral picWidth
        noise = noise2d perturbation p
    in  Vec2 0.5 0 +. perturbationStrength *. noise

gradientField2 :: Perlin -> Vec2 -> Vec2
gradientField2 perturbation v@(Vec2 x y) =
    let perturbationStrength = x / fromIntegral picWidth
        a = rad (10 * noiseValue perturbation (x, y, 32156498))
    in  Vec2 1 0 +. polar a (Distance perturbationStrength)

gradientField3 :: Perlin -> Vec2 -> Vec2
gradientField3 perturbation p@(Vec2 x y) =
    let perturbationStrength = x / fromIntegral picWidth
        noise (Vec2 x' y') = noiseValue perturbation (x' + 49156616, y' + 46216981, 321685163213)
        grad f v = 100 *. Vec2 (f (v +. Vec2 0.01 0) - f v) (f (v +. Vec2 0 0.01) - f v)
        Vec2 dx dy = noiseScale * perturbationStrength *. grad noise p
    in  Vec2 (1.5 - perturbationStrength) 0 +. Vec2 dy (-dx)

noise2d :: Perlin -> Vec2 -> Vec2
noise2d perturbation (Vec2 x y) = Vec2
    (noiseValue perturbation (x, y, 231356498))
    (noiseValue perturbation (x, y, 9872146164))

gradientDescent :: (Vec2 -> Vec2) -> Vec2 -> [Vec2]
gradientDescent gradient = go 0
  where
    go i s
        | i < 20000 = s : go (i+1) (s +. 0.1 *. gradient s)
        | otherwise = []
