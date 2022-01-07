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

-- Apply a transformation so the points are scaled and translated to fill the frame better.
fillFrameTransformation :: Int -> Int -> [Vec2] -> Render ()
fillFrameTransformation w h points = do
    let BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax) = boundingBox points

    let xRange = xMax - xMin
        yRange = yMax - yMin
        xScaleFactor = fromIntegral w / xRange
        yScaleFactor = fromIntegral h / yRange
        scaleFactor = min xScaleFactor yScaleFactor

    Cairo.scale scaleFactor scaleFactor
    Cairo.translate (-xMin) (-yMin)
    Cairo.scale 0.95 0.95

renderTwoBodyProblem :: Int -> Int -> Render ()
renderTwoBodyProblem w h = do
    let trajectory = takeWhile (\(t,_) -> t < 1000) planetTrajectory

    fillFrameTransformation w h [x | (_, (x, _)) <- trajectory]

    do save
       newPath
       arc 0 0 7 0 (2*pi)
       closePath
       setSourceRGB 0.880722 0.611041 0.142051
       fillPreserve
       setSourceRGB 0 0 0
       setLineWidth 1
       stroke
       restore

    do save
       newPath
       setLineWidth 1
       for_ trajectory $ \(_, (Vec2 x y, _)) -> lineTo x y
       stroke
       restore

    do save
       newPath
       let (_, (Vec2 x y, _)) = head planetTrajectory
       arc x y 4 0 (2*pi)
       closePath
       setSourceRGB 0.922526 0.385626 0.209179
       fillPreserve
       setSourceRGB 0 0 0
       setLineWidth 1
       setLineCap LineCapRound
       stroke
       restore
