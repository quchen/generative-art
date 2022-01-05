{-# LANGUAGE BangPatterns #-}

module Steps.C.PlanetMotion (planetMotion, planetTrajectory, boundingBox) where

import Steps.C.DifferentialEquation
import Graphics.Rendering.Cairo hiding (x,y)
import Data.List


planetTrajectory :: [(Vec2, Vec2, Double)]
planetTrajectory = rungeKutta4Solve dxdt dvdt x0 v0 t0 dt
  where
    dxdt, dvdt :: Vec2 -> Vec2 -> Double -> Vec2
    dvdt x v _t
      = let gravity = (- attraction / norm x ** 2.95) *. x
            friction = (-0.0001 * norm v) *. v
        in gravity +. friction
    dxdt _x v _t = v
    x0 = Vec2 100 0
    v0 = Vec2 4 4
    t0 = 0
    dt = 0.1

    attraction = 2200

boundingBox :: [Vec2] -> (Vec2, Vec2)
boundingBox [] = error "Bounding box of nothing"
boundingBox (p:oints) = foldl'
    (\(!(Vec2 xMinAcc yMinAcc), !(Vec2 xMaxAcc yMaxAcc)) (Vec2 x y) ->
        (Vec2 (min xMinAcc x) (min yMinAcc y), Vec2 (max xMaxAcc x) (max yMaxAcc y)))
    (p,p)
    oints

fillFrameTransformation :: Int -> Int -> [Vec2] -> Render ()
fillFrameTransformation w h points = do
    let (Vec2 xMin yMin, Vec2 xMax yMax) = boundingBox points

    let xRange = xMax - xMin
        yRange = yMax - yMin
        xScaleFactor = fromIntegral w / xRange
        yScaleFactor = fromIntegral h / yRange
        scaleFactor = min xScaleFactor yScaleFactor

    scale scaleFactor scaleFactor
    translate (-xMin) (-yMin)
    scale 0.95 0.95


planetMotion :: Int -> Int -> Render ()
planetMotion w h = do
    let trajectory = takeWhile (\(_,_,t) -> t < 1000) planetTrajectory

    fillFrameTransformation w h [x | (x, _, _) <- trajectory]

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
       sequence_ [lineTo x y | (Vec2 x y, _, _) <- trajectory]
       stroke
       restore

    do save
       newPath
       let (Vec2 x y, _, _) = head planetTrajectory
       arc x y 4 0 (2*pi)
       closePath
       setSourceRGB 0.922526 0.385626 0.209179
       fillPreserve
       setSourceRGB 0 0 0
       setLineWidth 1
       stroke
       restore
