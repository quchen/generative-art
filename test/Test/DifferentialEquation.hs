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

planetTrajectory :: [(Double, (Vec2, Vec2))]
planetTrajectory = rungeKuttaAdaptiveStep f y0 t0 dt0 tol
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
    let phaseDiagram = takeWhile (\(t,_) -> t < 1000) planetTrajectory

        trajectoryBoundingBox = boundingBox [x | (_t, (x,_v)) <- phaseDiagram]
                             <> boundingBox (Vec2 0 0) -- Don’t forget about the sun :-)
        canvasBoundingBox = boundingBox (Vec2 0 0, Vec2 (fromIntegral w) (fromIntegral h))
        scaleToCanvas = transformBoundingBox trajectoryBoundingBox canvasBoundingBox MaintainAspectRatio
        addSomeMargin = translateT (Vec2 10 10) <> scaleT 0.98 0.98
        transformNicely = Geometry.transform (addSomeMargin <> scaleToCanvas)
        trajectory = [(t, transformNicely x) | (t, (x,_v)) <- phaseDiagram]

    -- Paint sun
    do save
       newPath
       let Vec2 xSun ySun = transformNicely (Vec2 0 0)
       arc xSun ySun 7 0 (2*pi)
       closePath
       setSourceRGB 0.880722 0.611041 0.142051
       fillPreserve
       setSourceRGB 0 0 0
       setLineWidth 1
       stroke
       restore

    -- Paint trajectory
    do save
       newPath
       setLineWidth 1
       for_ trajectory $ \(_t, Vec2 x y) -> lineTo x y
       stroke
       restore

    -- Paint planet
    do save
       newPath
       let (_t, Vec2 x y) = head trajectory
       arc x y 4 0 (2*pi)
       closePath
       setSourceRGB 0.922526 0.385626 0.209179
       fillPreserve
       setSourceRGB 0 0 0
       setLineWidth 1
       setLineCap LineCapRound
       stroke
       restore
