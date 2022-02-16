module Main (main) where



import           Data.Maybe
import qualified Data.Vector              as V
import qualified Graphics.Rendering.Cairo as C
import           Math.Noise               (Perlin (..), getValue, perlin)
import           System.Random.MWC

import           Draw
import           Geometry                      as G
import           Geometry.Algorithms.Sampling
import qualified Geometry.Processes.FlowField  as ODE
import           Graphics.Rendering.Cairo
import           Numerics.DifferentialEquation
import           Numerics.VectorAnalysis



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

noiseScale :: Double
noiseScale = 251

noiseSeed :: Int
noiseSeed = 519496

main :: IO ()
main = withSurfaceAuto "out/vector_fields.svg" scaledWidth scaledHeight $ \surface -> C.renderWith surface $ do
    C.scale scaleFactor scaleFactor
    cairoScope $ do
        setColor white
        C.paint

    gen <- liftIO create
    startPoints <- liftIO $ G.transform (G.translate (Vec2 (-500) 0)) <$> poissonDisc PoissonDisc
        { width = picWidth + 500
        , height = picHeight
        , radius = 18
        , k = 4
        , gen = gen }

    cairoScope $ for_ startPoints $ \startPoint -> do
        thickness <- liftIO (uniformR (0.5, 1.5) gen)
        let trajectory = takeWhile (\(t, _) -> t <= 200) (fieldLine compositeField startPoint)
        drawFieldLine thickness trajectory
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
    noise = perlin { perlinFrequency = 1/noiseScale, perlinOctaves = 1, perlinSeed = noiseSeed }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)

gradientField :: Vec2 -> Vec2
gradientField p = noiseScale *. grad scalarField p

rotationField :: Vec2 -> Vec2
rotationField = G.transform (G.rotate (deg 90)) . gradientField

compositeField :: Vec2 -> Vec2
compositeField p@(Vec2 x y) = Vec2 1 0 +. 0.8 * perturbationStrength *. rotationField p
  where
    perturbationStrength = 0.7 * (1 + tanh (4 * (x / picWidth - 0.6))) * exp (-3 * (y / picHeight - 0.5)^2)

fieldLine
    :: (Vec2 -> Vec2)
    -> Vec2
    -> [(Double, Vec2)]
fieldLine f p0 = rungeKuttaAdaptiveStep (ODE.fieldLine f) p0 t0 dt0 tolNorm tol
  where
    t0 = 0
    dt0 = 1
    -- Decrease exponent for more accurate results
    tol = 1e-1
    tolNorm = norm
