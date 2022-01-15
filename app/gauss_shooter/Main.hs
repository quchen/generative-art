module Main (main) where

import           Data.Foldable
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector                     as V
import           Graphics.Rendering.Cairo        as C
import qualified System.Random.MWC               as Random
import qualified System.Random.MWC.Distributions as Random
import Data.Default.Class
import Data.Word
import Control.Parallel
import Control.Parallel.Strategies

import Draw
import Geometry as G
import Numerics.DifferentialEquation
import Numerics.VectorAnalysis

main :: IO ()
main = withSurfaceAuto "scratchpad/out.png" 1000 1000 $ \surface -> C.renderWith surface render

data SystemConfig = SystemConfig
    { _seed :: V.Vector Word32
    , _numHills :: Int
    , _sigmaHillDistribution :: Double
    , _numParticles  :: Int
    , _boundingBox :: BoundingBox

    , _sigmaHillWidth :: Double
    , _sigmaHillHeight :: Double
    , _muHillHeight :: Double
    }

instance Default SystemConfig where
    def = SystemConfig
        { _seed = V.fromList [1,3,5]

        , _boundingBox = BoundingBox (Vec2 (-500) (-500)) (Vec2 500 500)

        , _numHills = 200
        , _sigmaHillDistribution = 1000
        , _numParticles  = 100

        , _sigmaHillWidth = 100
        , _sigmaHillHeight = 3
        , _muHillHeight = 20
        }

systemSetup config@SystemConfig{..} = runST $ do
    gen <- Random.initialize _seed
    potential <- gaussianHillPotential config gen

    particleIcs <- do
        let mkParticleIc = do
                let x0 = Vec2 0 0
                v0 <- gaussianVec2 (Vec2 0 0) 3 gen
                pure (x0, v0)
        replicateM _numParticles mkParticleIc

    let odeSolutions =
            -- [ rungeKuttaConstantStep ode ic t0 stepSize
            [ rungeKuttaAdaptiveStep ode ic t0 dt0 toleranceNorm tolerance
            | ic <- particleIcs
            , let ode _t (x,v) = (v, 1000 *. negateV (grad potential x))
            , let t0 = 0
            , let dt0 = 1
            , let tolerance = 1e-2
            , let toleranceNorm (x,v) = sqrt (max (normSquare x) (normSquare v))
            ]

    let parFor = flip (parMap rseq)
        trajectories = parFor odeSolutions $ \odeSolution ->
            let getTrajectory sol = [x | (_t, (x, _v)) <- sol]
                timeCutoff cutoff = takeWhile (\(t, _) -> t < cutoff)
                spaceCutoff = takeWhile (\(_t, (Vec2 x y, _v)) -> -500 < x && x < 500 && -500 < y && y < 500)
                trajectory = getTrajectory (timeCutoff 10000 (spaceCutoff odeSolution))
            in trajectory

    pure trajectories

render :: Render ()
render = do
    let setup = systemSetup def
    C.translate 500 500
    -- cartesianCoordinateSystem

    for_ setup $ \trajectory -> do
        liftIO (putStrLn ("Trajectory length: " ++ show (length trajectory) ++ " segments"))
        let trajectory' = simplifyTrajectory (Distance 1) trajectory
        liftIO (putStrLn ("Simplified length: " ++ show (length trajectory') ++ " segments"))
        cairoScope $ do
            setSourceRGBA 0 0 0 0.2
            pathSketch trajectory'
            stroke
        pure ()

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
    hill <- replicateM _numHills $ do
        center <- gaussianVec2 (Vec2 0 0) _sigmaHillDistribution gen
        height <- Random.normal _muHillHeight _sigmaHillHeight gen
        pure (center, height)
    pure (\p -> sum' [height * gauss center _sigmaHillWidth p | (center, height) <- hill])

sum' :: [Double] -> Double
sum' = foldl' (+) 0
