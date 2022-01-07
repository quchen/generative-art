module Test.DifferentialEquation (tests) where

import Geometry
import Draw
import Geometry.Processes.DifferentialEquation
import Graphics.Rendering.Cairo as Cairo hiding (x, y)

planetTrajectory :: [(Double, (Vec2, Vec2))]
planetTrajectory = rungeKuttaAdaptiveStep f y0 t0 dt0 tolerance
  where
    f :: Double -> (Vec2, Vec2) -> (Vec2, Vec2)
    f _t (x, v)
      = let gravity = (- attraction / norm x ** 2.94) *. x
            attraction = 2200
            friction = (-0.0001 * norm v) *. v
            a = gravity +. friction
        in (v, a)
    y0 = (Vec2 100 0, Vec2 4 4)
    t0 = 0

    dt0 = 10
    tolerance = 0.001

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

planetMotion :: Int -> Int -> Render ()
planetMotion w h = do
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
