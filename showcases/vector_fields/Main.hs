module Main (main) where



import Control.Monad
import Graphics.Rendering.Cairo        as C hiding (x, y)
import System.Random.MWC
import System.Random.MWC.Distributions

import           Draw
import           Geometry                        as G
import           Geometry.Algorithms.PerlinNoise
import qualified Geometry.Processes.FlowField    as ODE
import           Numerics.DifferentialEquation
import           Numerics.Functions
import           Numerics.VectorAnalysis



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 400

-- Higher values yield lower-frequency noise
noiseScale :: Double
noiseScale = 0.5 * min picWidth picHeight

noiseSeed :: Int
noiseSeed = 519496

main :: IO ()
main = render "out/vector_fields.png" picWidth picHeight $ do
    cairoScope $ do
        setColor white
        C.paint

    gen <- liftIO create
    let startPoints = [Vec2 0 y | y <- [-50..picHeight+50]]

    cairoScope $ for_ startPoints $ \startPoint -> do
        thickness <- liftIO (uniformR (0.1, 1.0) gen)
        setLineWidth thickness
        colorValue <- liftIO $ do
            let Vec2 _ y = startPoint
            normal (y/picHeight) 0.3 gen
        setColor (rocket colorValue)
        let trajectory =
                takeWhile
                    (\(t, pos) -> t <= 1600 && pos `insideBoundingBox` (Vec2 (-50) (-50), Vec2 (picWidth+50) (picHeight+50)))
                    (fieldLine velocityField startPoint)
        drawFieldLine trajectory

drawFieldLine :: [(Double, Vec2)] -> Render ()
drawFieldLine ps = cairoScope $ do
    let polyLine = map snd ps
        simplified = simplifyTrajectoryRadial 3 polyLine
    when (not (null (drop 2 simplified))) $ do
        sketch (PolyBezier (bezierSmoothen simplified))
        stroke

-- 2D vector potential, which in 2D is umm well a scalar potential.
vectorPotential :: Vec2 -> Double
vectorPotential p = noiseScale *. perlin2 params p
  where
    params = PerlinParameters
        { _perlinFrequency   = 3/noiseScale
        , _perlinLacunarity  = 2
        , _perlinOctaves     = 1
        , _perlinPersistence = 0.5
        , _perlinSeed        = noiseSeed
        }

rotationField :: Vec2 -> Vec2
rotationField = curlZ vectorPotential

velocityField :: Vec2 -> Vec2
velocityField p@(Vec2 x y) = Vec2 1 0 +. perturbationStrength *. rotationField p
  where
    perturbationStrength =
        1.4
        * logisticRamp (0.6*picWidth) (picWidth/6) x
        * gaussianFalloff (0.5*picHeight) (0.4*picHeight) y

fieldLine
    :: (Vec2 -> Vec2)
    -> Vec2
    -> [(Double, Vec2)]
fieldLine f p0 = rungeKuttaAdaptiveStep (ODE.fieldLine f) p0 t0 dt0 tolNorm tol
  where
    t0 = 0
    dt0 = 1
    -- Decrease exponent for more accurate results
    tol = 1e-2
    tolNorm = norm
