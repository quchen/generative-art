{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where



import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Class
import Formatting (format, fixed, int, (%))
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC
import System.Random.MWC.Distributions

import Draw
import Draw.Plotting
import Geometry
import Geometry.Algorithms.Sampling
import Numerics.DifferentialEquation
import Physics
import Draw.Plotting.GCode (GCode(G01_LinearFeedrateMove))

main :: IO ()
main = do
    let particles = runST $ do
            gen <- create
            qs <- poissonDisc gen def
                { _poissonShape = BoundingBox (Vec2 50 50) (Vec2 550 350)
                , _poissonRadius = 30
                , _poissonK = 4
                }
            ps <- replicateM (length qs) $ gaussianVec2 zero 1 gen
            pure (NBody $ zipWith PhaseSpace ps qs)
        masses = pure 1
        externalPotential = harmonicPotential (34, 24) (Vec2 300 200)
        interactionPotential = coulombPotential 100
        toleranceNorm (NBody xs) = maximum (fmap (\PhaseSpace {..} -> max (norm p) (norm q)) xs)
        tolerance = 0.1
        initialStep = 1
        t0 = 0
        tmax = 100
        trajectories = getNBody $ traverse (\(t, pq) -> (t,) <$> pq) $ takeWhile ((<tmax) . fst) $
            rungeKuttaAdaptiveStep (const (nBody externalPotential interactionPotential masses)) particles t0 initialStep toleranceNorm tolerance

    render "out/brownian-motion.png" 600 400 $ do
        setColor white
        C.paint
        for_ trajectories $ \((_, PhaseSpace { q = q0 }) : trajectory) -> do
            moveToVec q0
            for_ trajectory $ \(t, PhaseSpace {..}) -> do
                lineToVec q
                setColor (black `withOpacity` (1 - t/tmax))
                C.stroke
                moveToVec q

    let feedrate = 12000
        settings = def { _feedrate = feedrate, _zDrawingHeight = -10, _zTravelHeight = 5 }
    writeGCodeFile "brownian-motion.g" $ runPlot settings $
        for_ (zip [1..] trajectories) $ \(i, trajectory) -> do
            let (_, q0) : tqs = (\(t, PhaseSpace {..}) -> (t, q)) <$> trajectory
            repositionTo q0
            penDown
            for_ tqs $ \(t, Vec2 x y) ->
                gCode [ G01_LinearFeedrateMove (Just feedrate) (Just x) (Just y) (Just ((t - tmax) / 10)) ]
            penUp
            when (i `mod` 20 == 0) $ withDrawingHeight 0 $ do
                repositionTo zero
                penDown
                dl <- gets _drawingDistance
                comment (format ("Sharpen pencil (" % int % ") at " % fixed 1 % "m") (i `div` 10) (dl/1000))
                pause PauseUserConfirm
                penUp

gaussianVec2
    :: Vec2 -- ^ Mean
    -> Double -- ^ Standard deviation
    -> GenST s
    -> ST s Vec2
gaussianVec2 (Vec2 muX muY) sigma gen = Vec2 <$> normal muX sigma gen <*> normal muY sigma gen
