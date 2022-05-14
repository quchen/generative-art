{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where



import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Class
import qualified Data.Vector as V
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

-- DIN A6 postcard format plus 5mm margin
picWidth, picHeight :: Num a => a
picWidth = 158
picHeight = 115

margin :: Vec2
margin = Vec2 5 5

-- DIN A6 postcard size with 5mm margin
canvas :: BoundingBox
canvas = boundingBox [margin, Vec2 picWidth picHeight -. margin]

main :: IO ()
main = for_
    [ (25, 1, "Compression")
    , (14.5, 2, "Deflation")
    , (20, 3, "Equilibrium")
    ] $ \(pressure, index, title) -> do
    let tmax = picWidth / 6
        trajectories = runSimulation pressure tmax
    render ("out/pressure-" ++ show index ++ ".svg") picWidth picHeight $ do
        cairoScope (setColor white >> C.paint)
        C.setLineWidth 0.2
        for_ trajectories $ \((_, PhaseSpace { q = q0 }) : trajectory) -> cairoScope $ do
            moveToVec q0
            for_ trajectory $ \(t, PhaseSpace {..}) -> do
                lineToVec q
                setColor (black `withOpacity` (1 - t/tmax))
                C.stroke
                moveToVec q
        sketch (boundingBoxPolygon canvas)
        C.stroke

    let feedrate = 12000
        settings = def
            { _feedrate = feedrate
            , _zDrawingHeight = -5
            , _zTravelHeight = 5
            }
    writeGCodeFile ("pressure-" ++ show index ++ ".g") $ runPlot settings $ do
        let caption = "Pressure " ++ show index ++ " - " ++ title
            textSettings = def
                { _textStartingPoint = Vec2 picWidth 0 +. transform (mirrorXCoords <> scale' 1 1.2) margin
                , _textHAlign = HRight
                , _textHeight = picHeight / 55
                }
            cutMark = Polyline [ Vec2 (picWidth - 5) picHeight, Vec2 picWidth picHeight, Vec2 picWidth (picHeight - 5) ]
        plot (plotText textSettings caption)
        plot cutMark
        for_ (zip [1..] trajectories) $ \(i, trajectory) -> do
            let (_, q0) : tqs = (\(t, PhaseSpace {..}) -> (t, q)) <$> trajectory
            repositionTo q0
            penDown
            for_ tqs $ \(t, Vec2 x y) ->
                gCode [ G01_LinearFeedrateMove (Just feedrate) (Just x) (Just y) (Just ((t/tmax - 1) * 10)) ]
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

runSimulation :: Double -> Double -> [[(Double, PhaseSpace Vec2)]]
runSimulation pressure tmax =
    let particles = runST $ do
            gen <- initialize (V.fromList [134])
            qs <- poissonDisc gen def
                { _poissonShape = boundingBox [4 *. margin, Vec2 picWidth picHeight -. 4 *. margin]
                , _poissonRadius = sqrt (picWidth * picHeight) / 18
                , _poissonK = 4
                }
            ps <- replicateM (length qs) $ gaussianVec2 zero 1 gen
            pure (NBody $ zipWith PhaseSpace ps qs)
        masses = pure 1
        externalPotential = harmonicPotential (picWidth / pressure, picHeight / pressure) (Vec2 (picWidth/2) (picHeight/2))
        interactionPotential = coulombPotential (picWidth / 6)
        toleranceNorm (NBody xs) = maximum (fmap (\PhaseSpace {..} -> max (norm p) (norm q)) xs)
        tolerance = 0.005
        initialStep = 1
        t0 = 0
        insideCanvas PhaseSpace{..} = q `insideBoundingBox` canvas
    in  fmap (takeWhile (insideCanvas . snd)) $ getNBody $ traverse (\(t, pq) -> (t,) <$> pq) $ takeWhile ((<tmax) . fst) $ --fmap (\(t, xs) -> (t - tmax, xs)) $ dropWhile ((<tmax) . fst) $
            rungeKuttaAdaptiveStep (const (nBody externalPotential interactionPotential masses)) particles t0 initialStep toleranceNorm tolerance
