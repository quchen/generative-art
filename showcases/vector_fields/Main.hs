{-# LANGUAGE RecordWildCards #-}
module Main (main) where



import           Control.Monad
import           Data.Maybe
import qualified Data.Vector              as V
import qualified Graphics.Rendering.Cairo as C
import           Math.Noise               (Perlin (..), getValue, perlin)
import           System.Random.MWC

import Draw
import Geometry                      as G
import Geometry.Algorithms.Sampling
import Graphics.Rendering.Cairo
import Numerics.DifferentialEquation



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

noiseScale :: Double
noiseScale = 251

-- Randomness seed for reproducible results
seed :: Int
seed = 519496

main :: IO ()
main = withSurfaceAuto "out/vector_fields.svg" scaledWidth scaledHeight $ \surface -> C.renderWith surface $ do
    C.scale scaleFactor scaleFactor
    cairoScope $ do
        C.setSourceRGB 1 1 1
        C.paint

    gen <- liftIO create
    startPoints <- liftIO $ G.transform (G.translate (Vec2 (-500) 0)) <$> poissonDisc PoissonDisc
        { width = picWidth + 500
        , height = picHeight
        , radius = 18
        , k = 4
        , .. }
    thicknesses <- liftIO $ replicateM 1000 (uniformR (0.5, 1.5) gen)

    for_ (zip startPoints (cycle thicknesses)) $ \(p, thickness) -> cairoScope $
        drawFieldLine thickness $ takeWhile ((<= 200) . fst) (fieldLine compositeField p)
  where
    scaleFactor = 0.39
    scaledWidth = round (picWidth * scaleFactor)
    scaledHeight = round (picHeight * scaleFactor)

drawFieldLine :: Double -> [(Double, Vec2)] -> Render ()
drawFieldLine _ [] = pure ()
drawFieldLine thickness ps = cairoScope $ do
    let ps' = V.toList (bezierSmoothen (V.fromList (snd <$> ps)))
        time = fst <$> ps
    for_ (zip time ps') $ \(t, p) -> do
        setLineCap LineCapRound
        setLineWidth (thickness * (t/100))
        bezierSegmentSketch p
        stroke

scalarField :: Vec2 -> Double
scalarField = noise2d . G.transform (G.scale' 1 2)
  where
    noise = perlin { perlinFrequency = 1/noiseScale, perlinOctaves = 1, perlinSeed = seed }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)

gradientField :: Vec2 -> Vec2
gradientField p = noiseScale *. grad scalarField p
  where
    grad f v = 100 *. Vec2 (f (v +. Vec2 0.01 0) - f v) (f (v +. Vec2 0 0.01) - f v)

rotationField :: Vec2 -> Vec2
rotationField = G.transform (G.rotate (deg 90)) . gradientField

compositeField :: Vec2 -> Vec2
compositeField p@(Vec2 x y) = Vec2 1 0 +. 0.8 * perturbationStrength *. rotationField p
  where
    perturbationStrength = 0.7 * (1 + tanh (4 * (x / picWidth - 0.6))) * exp (-3 * (y / picHeight - 0.5)^2)

fieldLine :: (Vec2 -> Vec2) -> Vec2 -> [(Double, Vec2)]
fieldLine vectorField p0 = rungeKuttaConstantStep ode p0 t0 dt
  where
    ode _t x = vectorField x
    t0 = 0
    -- high quality: 10, fast: 50
    dt = 50
