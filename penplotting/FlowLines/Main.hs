module Main (main) where



import           Control.Monad
import           Data.List
import qualified Data.Text.Lazy.IO        as T
import qualified Graphics.Rendering.Cairo as C


import qualified Data.Set                        as S
import           Data.Vector                     (Vector)
import           Draw
import           Draw.Plotting
import           Geometry                        as G
import           Geometry.Algorithms.PerlinNoise
import qualified Geometry.Processes.FlowField    as ODE
import           Graphics.Rendering.Cairo
import           Numerics.DifferentialEquation
import           Numerics.Functions
import           Numerics.VectorAnalysis
import           Data.Coerce



a4width_mm, a4height_mm :: Num a => a
a4width_mm = 297
a4height_mm = 210

width_mm, height_mm :: Num a => a
width_mm = a4width_mm
height_mm = a4height_mm

drawBB :: BoundingBox
drawBB = boundingBox (Vec2 10 10, Vec2 (width_mm-10) (height_mm-10))

-- Higher values yield lower-frequency noise
noiseScale :: Double
noiseScale = 0.5 * min width_mm height_mm

noiseSeed :: Int
noiseSeed = 519496

main :: IO ()
main = do
    render "out/vector_fields.png" width_mm height_mm $ do
        cairoScope $ do
            setColor white
            C.paint

        cairoScope $ do
            setLineWidth 1
            for_ geometry drawFieldLine

    let drawing = sequence
            [ plot (Polyline part)
            | trajectory <- geometry
            , part <- splitIntoInsideParts trajectory ]

        plottingSettings = def
            { _previewBoundingBox = Just (boundingBox geometry)
            , _feedrate = Just 1000
            }

    T.putStrLn (runPlot plottingSettings drawing)

geometry :: [Polyline Vector]
geometry =
    let startPoints = [Vec2 0 y | y <- [-50, -45..height_mm+50]]
        mkTrajectory start =
              Polyline
            . map (\(_t, pos) -> pos)
            . takeWhile
                (\(t, pos) -> t <= 400 && pos `insideBoundingBox` (Vec2 (-50) (-50), Vec2 (width_mm+50) (height_mm+50)))
            $ fieldLine velocityField start
    in (coerce . minimizePenHovering . S.fromList . concatMap (splitIntoInsideParts . mkTrajectory)) startPoints

drawFieldLine :: Polyline Vector -> Render ()
drawFieldLine (Polyline polyLine) = cairoScope $ do
    let simplified = simplifyTrajectoryRadial 3 polyLine
    unless (null (drop 2 simplified)) $ do
        sketch (bezierSmoothen simplified)
        stroke

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

splitIntoInsideParts :: Sequential list => Polyline list -> [[Vec2]]
splitIntoInsideParts (Polyline xs) = filter (\(x:_) -> x `insideBoundingBox` drawBB) . groupOn (\p -> insideBoundingBox p drawBB) . toList $ xs

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
        * logisticRamp (0.6*width_mm) (width_mm/6) x
        * gaussianFalloff (0.5*height_mm) (0.4*height_mm) y

fieldLine
    :: (Vec2 -> Vec2)
    -> Vec2
    -> [(Double, Vec2)]
fieldLine f p0 = rungeKuttaAdaptiveStep (ODE.fieldLine f) p0 t0 dt0 tolNorm tol
  where
    t0 = 0
    dt0 = 1
    -- Decrease exponent for more accurate results
    tol = 1e-3
    tolNorm = norm
