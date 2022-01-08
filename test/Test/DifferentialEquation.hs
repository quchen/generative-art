{-# LANGUAGE RecordWildCards #-}

module Test.DifferentialEquation (tests) where

import Geometry
import Draw
import Geometry.Processes.DifferentialEquation
import Graphics.Rendering.Cairo as Cairo hiding (x, y)
import Data.Foldable
import Test.Common
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Differential equations"
    [ testGroup "Visual"
        [ twoBodyProblem
        , doublePendulum
        ]
    ]

twoBodyProblem :: TestTree
twoBodyProblem = testCase "Two-body problem" (renderAllFormats 400 400 "test/out/differential_equations/1_two_body_problem" (renderTwoBodyProblem 400 400))

planetPhaseDiagram :: [(Double, (Vec2, Vec2))]
planetPhaseDiagram = rungeKuttaAdaptiveStep f y0 t0 dt0 tol
  where
    f :: Double -> (Vec2, Vec2) -> (Vec2, Vec2)
    f _t (x, v)
        -- Gravity is a bit weaker with r^1.94 instead of r^2 falloff to make the picture more interesting.
        -- Also some tiny friction for the same reason.
      = let gravity = (- attraction / let Distance r = norm x in r ** 2.94) *. x
            attraction = 2200
            friction = (-0.0001 * let Distance d = norm v in d) *. v
            a = gravity +. friction
        in (v, a)
    y0 = (Vec2 100 0, Vec2 4 4)
    t0 = 0

    dt0 = 10
    tol = 0.001

renderTwoBodyProblem :: Int -> Int -> Render ()
renderTwoBodyProblem w h = do
    let phaseDiagram' = takeWhile (\(t,_) -> t < 1000) planetPhaseDiagram

        transformNicely
          = let trajectoryBoundingBox = boundingBox [x | (_t, (x,_v)) <- phaseDiagram']
                                     <> boundingBox (Vec2 0 0) -- Don’t forget about the sun :-)
                canvasBoundingBox = boundingBox (Vec2 10 10, Vec2 (fromIntegral w - 10) (fromIntegral h - 10))
                scaleToCanvas = transformBoundingBox trajectoryBoundingBox canvasBoundingBox MaintainAspectRatio
            in Geometry.transform scaleToCanvas
        planetTrajectory = [(t, transformNicely x) | (t, (x,_v)) <- phaseDiagram']
        sun = transformNicely (Vec2 0 0)

    let paintSun = restoreStateAfter $ do
            newPath
            circleSketch sun (Distance 16)
            setLineWidth 2
            mmaColor 1 1
            fillPreserve
            setSourceRGB 0 0 0
            stroke

        paintTrajectory = restoreStateAfter $ do
            setLineWidth 1.5
            for_ planetTrajectory $ \(_t, Vec2 x y) -> lineTo x y
            stroke

        paintPlanet = restoreStateAfter $ do
            let (_t0, planet) = head planetTrajectory
            circleSketch planet (Distance 8)
            mmaColor 3 1
            fillPreserve
            setSourceRGB 0 0 0
            setLineWidth 2
            stroke

    paintSun
    paintTrajectory
    paintPlanet

doublePendulum :: TestTree
doublePendulum = testCase "Double pendulum" (renderAllFormats 400 400 "test/out/differential_equations/2_double_pendulum" (renderDoublePendulum 400 400))

data DoublePendulum = DoublePendulum {
      _m1 :: Double
    , _m2 :: Double
    , _l1 :: Double
    , _l2 :: Double
    , _g :: Double
    , _y0 :: ((Double, Double), (Double, Double))
}

solveDoublePendulum :: DoublePendulum -> [(Double, ((Double, Double), (Double, Double)))]
solveDoublePendulum DoublePendulum{..} = rungeKuttaAdaptiveStep f _y0 t0 dt tol
  where
    f :: Double -> ((Double, Double), (Double, Double)) -> ((Double, Double), (Double, Double))
    f _t ((theta1, theta2), (omega1, omega2))
      = (
            (
                omega1
            ,
                omega2
            ),(
                (- _g * (2*_m1 + _m2) * sin theta1 - _m2 * _g * sin (theta1 - 2*theta2) - 2 * sin (theta1 - theta2) * _m2 * (omega2**2 * _l2 + omega1**2 * _l1 * cos (theta1 - theta2)))
                / --------------------------------------------------------------------------------------------------------------------------------------------------------------
                (_l1 * (2*_m1 + _m2 + _m2 * cos(2*theta1 - 2*theta2)))
            ,
                (2 * sin(theta1 - theta2) * (omega1**2 * _l1 * (_m1 + _m2) + _g * (_m1 + _m2) * cos theta1 + omega2**2 * _l2 * _m2 * cos (theta1 - theta2)))
                / ----------------------------------------------------------------------------------------------------------------------------------
                (_l1 * (2*_m1 + _m2 + _m2 * cos(2*theta1 - 2*theta2)))
            )
        )

    t0 = 0


    dt = 0.001
    tol = 0.001

system :: DoublePendulum
system = DoublePendulum {
      _m1 = 1
    , _m2 = 1
    , _l1 = 100
    , _l2 = 100
    , _g = 9.81
    , _y0 = ((120 * 2*pi/360, 120 * 2*pi/360), (0, 0))
}

renderDoublePendulum :: Int -> Int -> Render ()
renderDoublePendulum _w _h = do
    let trajectory = takeWhile (\(t, _x) -> t < 2000) (doublePendulumTrajectory system)
        scaleToCanvas = Geometry.transform (translateT (Vec2 200 200))
        transformedTrajectory = [(t, scaleToCanvas x) | (t,x) <- trajectory]
        bezierSmoothTrajectory = bezierSmoothen [x | (_, x) <- transformedTrajectory]

    restoreStateAfter $ do
        let (_t0, start) = head transformedTrajectory
        moveToVec start
        setLineWidth 1
        for_ (zip transformedTrajectory bezierSmoothTrajectory) $ \((t, _), bezier) -> do
            mmaColor 1 (exp (-t / 500))
            bezierSegmentSketch bezier
            stroke

doublePendulumTrajectory :: DoublePendulum -> [(Double, Vec2)]
doublePendulumTrajectory sys =
    let solution = solveDoublePendulum sys

        -- Position of the lower pendulum
        v2 (t, ((theta1, theta2), _omegas))
          = let DoublePendulum{..} = sys
                x1 = _l1 * sin theta1
                y1 = _l1 * cos theta1
                x2 = x1 + _l2 * sin theta2
                y2 = y1 + _l2 * cos theta2
            in (t, Vec2 x2 y2)
    in map v2 solution
