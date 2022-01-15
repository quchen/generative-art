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
    , _particleMass :: Double

    , _sigmaHillWidth :: Double
    , _sigmaHillHeight :: Double
    , _muHillHeight :: Double
    }

instance Default SystemConfig where
    def = SystemConfig
        { _seed = V.fromList [135,9,1,2,31,3,654,4,8,39,45]

        , _boundingBox = boundingBox (Vec2 (-500) (-500), Vec2 500 500)

        , _numHills = 500
        , _sigmaHillDistribution = 700
        , _numParticles  = 10000
        , _particleMass = 1

        , _sigmaHillWidth = 10
        , _sigmaHillHeight = 5
        , _muHillHeight = 70
        }

initializeGen
    :: SystemConfig
    -> ST s (Random.Gen s)
initializeGen SystemConfig{..} = do
    gen <- Random.initialize _seed
    _ <- fmap (\warmupGenerator -> warmupGenerator :: [Int])
              (replicateM 10000 (Random.uniform gen))
    pure gen

systemSetup config@SystemConfig{..} = runST $ do
    gen <- initializeGen config

    (potential, hills) <- gaussianHillPotential config gen

    particleIcs <- do
        let mkParticleIc = do
                let x0 = Vec2 0 0
                a <- Random.uniformRM (0, 360) gen
                let Line _ v0 = angledLine (Vec2 0 0) (deg a) (Distance 10)
                -- v0 <- gaussianVec2 (Vec2 0 0) 1 gen
                pure (x0, v0)
        replicateM _numParticles mkParticleIc

    let odeSolutions =
            [ rungeKuttaAdaptiveStep ode ic t0 dt0 toleranceNorm tolerance
            | ic <- particleIcs
            , let ode _t (x,v) = (v, negateV (grad potential x) /. _particleMass)
            , let t0 = 0
            , let dt0 = 1
            , let tolerance = 1e-2
            , let toleranceNorm (x,v) = sqrt (max (normSquare x) (normSquare v))
            ]

    let parFor = flip (parMap rseq)
        trajectories = parFor odeSolutions $ \odeSolution ->
            let getTrajectory sol = [x | (_t, (x, _v)) <- sol]
                timeCutoff cutoff = takeWhile (\(t, _) -> t < cutoff)
                spaceCutoff = takeWhile (\(_t, (x, _v)) -> overlappingBoundingBoxes x _boundingBox)
                trajectory = getTrajectory (timeCutoff 1000 (spaceCutoff odeSolution))
            in trajectory

    pure (trajectories, hills)

render :: Render ()
render = do
    let (trajectories, hills) = systemSetup def
    C.translate 500 500
    -- cartesianCoordinateSystem
    cairoScope $ do
        setSourceRGB 1 1 1
        paint

    setLineWidth 1
    for_ hills $ \(center, radius) -> cairoScope $ do
            mmaColor 1 1
            circleSketch center (Distance radius)
            stroke
    for_ (trajectories) $ \trajectory -> do
        let trajectory' = simplifyTrajectory (Distance 1) trajectory
        liftIO (putStrLn ("Trajectory length: " ++ show (length trajectory) ++ " segments, simplified: " ++ show (length trajectory')))
        cairoScope $ do
            mmaColor 0 0.01
            pathSketch trajectory'
            stroke
        pure ()

makeHill
    :: Vec2   -- ^ Mean
    -> Double -- ^ width
    -> Vec2   -- ^ x
    -> Double
makeHill mu sigma p = exp (- normSquare (p -. mu) / (2*sigma^2))

gaussianVec2
    :: Vec2 -- ^ Mean
    -> Double -- ^ Standard deviation
    -> Random.GenST s
    -> ST s Vec2
gaussianVec2 (Vec2 muX muY) sigma gen = Vec2 <$> Random.normal muX sigma gen <*> Random.normal muY sigma gen

gaussianHillPotential
    :: SystemConfig
    -> Random.Gen s
    -> ST s (Vec2 -> Double, [(Vec2, Double)])
gaussianHillPotential SystemConfig{..} gen = do
    hills <- do
        hills' <- replicateM _numHills $ do
            center <- gaussianVec2 (Vec2 0 0) _sigmaHillDistribution gen
            height <- Random.normal _muHillHeight _sigmaHillHeight gen
            pure (center, height)
        let removeOutliers = filter (\(center, _) -> overlappingBoundingBoxes center (G.transform (G.scale 1.5) _boundingBox))
        pure (removeOutliers hills')
    pure (\p -> sum' [height * makeHill center _sigmaHillWidth p | (center, height) <- hills]
         , [(center, _sigmaHillWidth) | (center, _) <- hills]
         )

sum' :: [Double] -> Double
sum' = foldl' (+) 0
