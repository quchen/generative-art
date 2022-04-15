module Main (main) where



import Control.Monad
import Control.Monad.ST
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC
import System.Random.MWC.Distributions

import Draw
import Geometry
import Geometry.Algorithms.Sampling
import Numerics.DifferentialEquation
import Physics

main :: IO ()
main = do
    let particles = runST $ do
            gen <- create
            qs <- poissonDisc gen def
                { _poissonShape = BoundingBox zero (Vec2 600 400)
                , _poissonRadius = 30
                , _poissonK = 4
                }
            ps <- replicateM (length qs) $ gaussianVec2 zero 4 gen
            pure (NBody $ zipWith PhaseSpace ps qs)
        masses = pure 1
        interactionPotential = coulombPotential 100
        toleranceNorm (NBody xs) = maximum (fmap (\PhaseSpace {..} -> max (norm p) (norm q)) xs)
        tolerance = 0.1
        initialStep = 1
        t0 = 0
        trajectories = traverse snd $ takeWhile ((<100) . fst) $
            rungeKuttaAdaptiveStep (const (nBody zero interactionPotential masses)) particles t0 initialStep toleranceNorm tolerance

    render "out/brownian-motion.png" 600 400 $ do
        setColor white
        C.paint
        for_ (zip [0..] (getNBody trajectories)) $ \(i, trajectory) -> do
            sketch (fmap q trajectory)
            setColor (mathematica97 i)
            C.stroke

gaussianVec2
    :: Vec2 -- ^ Mean
    -> Double -- ^ Standard deviation
    -> GenST s
    -> ST s Vec2
gaussianVec2 (Vec2 muX muY) sigma gen = Vec2 <$> normal muX sigma gen <*> normal muY sigma gen
