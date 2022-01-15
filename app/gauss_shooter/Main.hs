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
    , _sigmaCharge :: Double
    , _muCharge :: Double
    }

instance Default SystemConfig where
    def = SystemConfig
        { _seed = V.fromList [13,5,9,1,39,45]

        , _boundingBox = boundingBox (Vec2 (-500) (-500), Vec2 500 500)

        , _numHills = 5000
        , _sigmaHillDistribution = 1500
        , _numParticles  = 10000
        , _particleMass = 1000

        , _sigmaHillWidth = 10
        , _sigmaCharge = 2e3
        , _muCharge = 1e3
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

    (potential, coulombWells) <- potentials config gen

    particleIcs <- do
        let mkParticleIc = do
                let x0 = Vec2 0 0
                a <- Random.uniformRM (0, 360) gen
                let Line _ v0 = angledLine (Vec2 0 0) (deg a) (Distance 1)
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

    let trajectoryThunks = flip map odeSolutions $ \odeSolution ->
            let getTrajectory sol = [x | (_t, (x, _v)) <- sol]
                timeCutoff = takeWhile (\(t, _) -> t < 1000)
                spaceCutoff = takeWhile (\(_t, (x, _v)) -> overlappingBoundingBoxes x _boundingBox)
                simplify = simplifyTrajectory (Distance 1)
            in (simplify . getTrajectory . timeCutoff . spaceCutoff) odeSolution
        !trajectoriesNF = trajectoryThunks `using` parListChunk 64 rdeepseq

    pure (trajectoriesNF, coulombWells)

render :: Render ()
render = do
    let (trajectories, hills) = systemSetup def
    C.translate 500 500
    -- cartesianCoordinateSystem
    cairoScope $ do
        setSourceRGB 1 1 1
        paint

    setLineWidth 1
    for_ hills $ \(center, charge) -> cairoScope $ do
            mmaColor 1 1
            circleSketch center (Distance (log charge))
            stroke
    for_ (zip [1..] trajectories) $ \(i, trajectory) -> do
        liftIO (putStrLn ("Paint trajectory " ++ show i))
        cairoScope $ do
            mmaColor 3 0.01
            pathSketch trajectory
            stroke
        pure ()

gaussianVec2
    :: Vec2 -- ^ Mean
    -> Double -- ^ Standard deviation
    -> Random.GenST s
    -> ST s Vec2
gaussianVec2 (Vec2 muX muY) sigma gen = Vec2 <$> Random.normal muX sigma gen <*> Random.normal muY sigma gen

potentials
    :: SystemConfig
    -> Random.Gen s
    -> ST s (Vec2 -> Double, [(Vec2, Double)])
potentials SystemConfig{..} gen = do
    hills <- do
        hills' <- replicateM _numHills $ do
            center <- gaussianVec2 (Vec2 0 0) _sigmaHillDistribution gen
            charge <- Random.normal _muCharge _sigmaCharge gen
            pure (center, charge)
        let removeOutliers = filter (\(center, _) -> overlappingBoundingBoxes center (G.transform (G.scale 1.1) _boundingBox))
                           . filter (\(center, _) -> let Distance d = norm center in d > 70)
        pure (removeOutliers hills')
    pure (\p -> sum' [charge / (let Distance d = norm (p -. center) in d) | (center, charge) <- hills]
         , hills
         )

sum' :: [Double] -> Double
sum' = foldl' (+) 0
