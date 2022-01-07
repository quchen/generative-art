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
        ]
    ]

twoBodyProblem :: TestTree
twoBodyProblem = testCase "Two-body problem" (renderAllFormats 800 800 "test/out/differential_equations/1_two_body_problem" (renderTwoBodyProblem 800 800))

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
                canvasBoundingBox = boundingBox (Vec2 0 0, Vec2 (fromIntegral w) (fromIntegral h))
                scaleToCanvas = transformBoundingBox trajectoryBoundingBox canvasBoundingBox MaintainAspectRatio
                addSomeMargin = translateT (Vec2 10 10) <> scaleT 0.98 0.98
            in Geometry.transform (addSomeMargin <> scaleToCanvas)
        planetTrajectory = [(t, transformNicely x) | (t, (x,_v)) <- phaseDiagram']
        sun = transformNicely (Vec2 0 0)

    let paintSun = restoreStateAfter $ do
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
