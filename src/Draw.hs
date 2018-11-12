{-# LANGUAGE RecordWildCards #-}

module Draw where



import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable
import Graphics.Rendering.Cairo hiding (x, y)

import Geometry



hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v a = setSourceRGBA channelRed channelGreen channelBlue a
    where RGB{..} = hsv h s v

lineSketch :: Line -> Render ()
lineSketch (Line (Vec2 x1 y1) (Vec2 x2 y2)) = moveTo x1 y1 >> lineTo x2 y2

circleSketch :: Vec2 -> Distance -> Render ()
circleSketch (Vec2 x y) (Distance r) = arc x y r 0 (2*pi)

crossSketch :: Vec2 -> Distance -> Render ()
crossSketch center (Distance r) = do
    let lowerRight = rotateAround center (deg 45) (center `addVec2` Vec2 r 0)
        line1 = angledLine lowerRight (deg (45+180)) (Distance (2*r))
        line2 = rotateAround center (deg 90) line1
    lineSketch line1
    lineSketch line2

arcSketch :: Vec2 -> Distance -> Angle -> Angle -> Render ()
arcSketch (Vec2 x y) (Distance r) (Angle angleStart) (Angle angleEnd)
  = arc x y r angleStart angleEnd

polygonSketch :: Polygon -> Render ()
polygonSketch (Polygon []) = pure ()
polygonSketch (Polygon (Vec2 x y : vecs)) = do
    moveTo x y
    for_ vecs (\(Vec2 x' y') -> lineTo x' y')
    closePath

-- | Draw a caresian coordinate system in range (x,x') (y,y')
cartesianCoordinateSystemDraw :: (Int, Int) -> (Int, Int) -> Render ()
cartesianCoordinateSystemDraw (minX, maxX) (minY, maxY) = do
    let vec2 x y = Vec2 (fromIntegral x) (fromIntegral y)
    setLineWidth 1
    hsva 0 0 0 0.8
    sequence_ [ lineSketch (Line (vec2 x minY) (vec2 x maxY))
              | x <- [minX, minX+100 .. maxX] ]
    sequence_ [ lineSketch (Line (vec2 minX y) (vec2 maxX y))
              | y <- [minY, minY+100 .. maxY] ]
    stroke

    hsva 0 0 0 0.5
    sequence_ [ lineSketch (Line (vec2 x minY) (vec2 x maxY))
              | x <- [minX, minX+10 .. maxX]
              , mod x 100 /= 0 ]
    sequence_ [ lineSketch (Line (vec2 minX y) (vec2 maxX y))
              | y <- [minY, minY+10 .. maxY]
              , mod y 100 /= 0]
    stroke

radialCoordinateSystemDraw :: Vec2 -> Int -> Render ()
radialCoordinateSystemDraw center maxR = do
    let distance = Distance . fromIntegral
    setLineWidth 1
    hsva 0 0 0 1
    sequence_ [ circleSketch center (distance r) >> stroke
              | r <- [100, 200 .. maxR] ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) (distance maxR)) >> stroke
              | angle <- init [0, 45 .. 360 :: Int] ]

    hsva 0 0 0 0.5
    sequence_ [ circleSketch center (distance r) >> stroke
              | r <- [25, 50 .. maxR]
              , mod r 100 /= 0 ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) (distance maxR)) >> stroke
              | angle <- init [0, 15 .. 360 :: Int]
              , mod angle 45 /= 0 ]
