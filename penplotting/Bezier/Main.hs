module Main (main) where



import Control.Monad
import Data.List
import System.Random.MWC
import Text.Printf

import Draw
import Draw.Plotting
import Geometry                         as G
import Geometry.Chaotic
import Geometry.Algorithms.PerlinNoise
import qualified Geometry.Processes.FlowField    as ODE
import Numerics.DifferentialEquation
import Numerics.VectorAnalysis



picWidth, picHeight :: Num a => a
picWidth = 500
picHeight = 700

canvas :: BoundingBox
canvas = boundingBox [Vec2 50 50, Vec2 (picWidth - 50) (picHeight - 50)]

-- Higher values yield lower-frequency noise
noiseScale :: Double
noiseScale = 1

seeds :: [Int]
seeds = [23, 7, 16]

main :: IO ()
main = for_ (zip [1 :: Int ..] seeds) $ \(i, seed) -> do
    gen <- initializeMwc seed
    initialPoints <- replicateM 4 $
        uniformRM (zero, Vec2 picWidth picHeight) gen
    let timeEvolution
            = fitToCanvas
            . fmap bezierSmoothenOpen
            . transpose
            . fmap (take 50 . fmap snd . spaced 2000 . fieldLine (rotationField seed))
            $ initialPoints
        plottingSettings = def
            { _feedrate = 10000
            , _zTravelHeight = 5
            , _zDrawingHeight = -2
            , _canvasBoundingBox = Just (boundingBox [zero, Vec2 picWidth picHeight])
            }
        plotResult = runPlot plottingSettings $ for_ timeEvolution plot
    renderPreview (printf "out/bezier%i.png" i) plotResult

-- 2D vector potential, which in 2D is umm well a scalar potential.
vectorPotential :: Int -> Vec2 -> Double
vectorPotential seed p = noiseScale *. perlin2 params p
  where
    params = PerlinParameters
        { _perlinFrequency   = 3 / (noiseScale * min picWidth picHeight)
        , _perlinLacunarity  = 2
        , _perlinOctaves     = 1
        , _perlinPersistence = 0.5
        , _perlinSeed        = seed
        }

rotationField :: Int -> Vec2 -> Vec2
rotationField seed = curlZ (vectorPotential seed)

fieldLine
    :: (Vec2 -> Vec2)
    -> Vec2
    -> [(Double, Vec2)]
fieldLine f p0 = rungeKuttaConstantStep (ODE.fieldLine f) p0 t0 dt
  where
    t0 = 0
    dt = 10

spaced :: Double -> [(Double, a)] -> [(Double, a)]
spaced dt = go 0
  where
    go _ [] = []
    go t0 ((t, a) : xs)
        | t < t0 = go t0 xs
        | t > t0 + dt = (t, a) : go (t0 + dt) ((t, a) : xs)
        | otherwise = (t, a) : go (t0 + dt) xs

fitToCanvas :: (HasBoundingBox geo, Transform geo) => geo -> geo
fitToCanvas geo = G.transform (transformBoundingBox geo canvas def) geo
