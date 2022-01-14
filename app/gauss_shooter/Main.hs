module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.List
import qualified Data.Vector                     as V
import           Graphics.Rendering.Cairo        as C
import qualified System.Random.MWC               as Random
import qualified System.Random.MWC.Distributions as Random
import Data.Default.Class

import Draw
import Geometry as G
import Numerics.DifferentialEquation
import Numerics.VectorAnalysis

main :: IO ()
main = withSurfaceAuto "scratchpad/out.png" 1000 1000 $ \surface -> C.renderWith surface render

data SystemConfig = SystemConfig
    { _dummy :: ()
    , _numHills :: Int
    , _sigmaHillCenter :: Double
    , _sigmaHill :: Double
    , _numParticles  :: Int
    }

instance Default SystemConfig where
    def = SystemConfig
        { _dummy = ()
        , _numHills = 100
        , _sigmaHillCenter = 1000
        , _sigmaHill = 100
        , _numParticles  = 10
        }

systemSetup config = runST $ do
    let seedVec = V.fromList [1,2,3]
    gen <- Random.initialize seedVec
    potential <- gaussianHillPotential config gen

    particleIcs <- do
        let mkParticleIc = do
                let x0 = Vec2 0 0
                v0 <- gaussianVec2 (Vec2 0 0) 1 gen
                pure (x0, v0)
        replicateM (_numParticles config) mkParticleIc

    let trajectories =
            [ rungeKuttaConstantStep ode ic t0 stepSize
            | ic <- particleIcs
            , let ode _t (x,v) = (v, negateV (grad potential x))
            , let t0 = 0
            , let stepSize = 1e-1
            ]

    pure trajectories

render :: Render ()
render = do
    let _ = systemSetup
    cartesianCoordinateSystem

gauss
    :: Vec2   -- ^ Mean
    -> Double -- ^ Standard deviation
    -> Vec2   -- ^ x
    -> Double
gauss mu sigma p = 1/(sqrt(2*pi)*sigma) * exp (- normSquare (p -. mu) / (2*sigma^2))

gaussianVec2
    :: Vec2 -- ^ Mean
    -> Double -- ^ Standard deviation
    -> Random.GenST s
    -> ST s Vec2
gaussianVec2 (Vec2 muX muY) sigma gen = Vec2 <$> Random.normal muX sigma gen <*> Random.normal muY sigma gen

gaussianHillPotential
    :: SystemConfig
    -> Random.Gen s
    -> ST s (Vec2 -> Double)
gaussianHillPotential SystemConfig{..} gen = do
    hillCenters <- replicateM _numHills (gaussianVec2 (Vec2 0 0) _sigmaHillCenter gen)
    pure (\p -> sum' [gauss center _sigmaHill p | center <- hillCenters])

sum' :: [Double] -> Double
sum' = foldl' (+) 0
