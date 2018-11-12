module Visual.IntersectionLL (tests) where

import Graphics.Rendering.Cairo hiding (x,y)

import Draw
import Geometry

import Visual.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testCase "Line-line intersection" testIntersectionLL

testIntersectionLL :: IO ()
testIntersectionLL = renderAllFormats 580 480 "test/out/intersection" (do
    testVirtual1
    testVirtual2
    testVirtualInL
    testVirtualR
    testReal1
    testReal2 )
  where
    testVirtual1 = testDraw
        (angledLine (Vec2 50 190) (Angle ( pi/6)) (Distance 100))
        (angledLine (Vec2 50 300) (Angle (-pi/6)) (Distance 100))
    testVirtual2 = testDraw
        (move (Vec2 30 380) (moveRad (Angle (-pi/6)) (Distance 20) (angledLine (Vec2 0 0) (Angle (-pi/6)) (Distance 100))))
        (move (Vec2 30 380) (moveRad (Angle ( pi/6)) (Distance 20) (angledLine (Vec2 0 0) (Angle ( pi/6)) (Distance 100))))
    testVirtualInL = testDraw
        (angledLine (Vec2 300 180) (Angle      0) (Distance 100))
        (angledLine (Vec2 350 200) (Angle (pi/2)) (Distance  50))
    testVirtualR = testDraw
        (angledLine (Vec2 370 330) (Angle      0) (Distance  50))
        (angledLine (Vec2 350 280) (Angle (pi/2)) (Distance 100))
    testReal1 = testDraw
        (Line (Vec2 10  10) (Vec2 220 190))
        (Line (Vec2 270 50) (Vec2  30 160))
    testReal2 = testDraw
        (move (Vec2 320 10) (Line (Vec2 0   0) (Vec2 120 120)))
        (move (Vec2 320 10) (Line (Vec2 120 0) (Vec2 0   120)))

testDraw :: Line -> Line -> Render ()
testDraw line1 line2 = do
    let (point, angle, ty) = intersectionLL line1 line2

    setLineWidth 1
    hsva 0 1 0.7 1
    arrowSketch line1
    stroke
    hsva 60 1 0.7 1
    arrowSketch line2
    stroke

    hsva 120 1 0.7 1
    circleSketch point (Distance 3)
    fill

    hsva 180 1 0.7 1
    arcSketch point (Distance 10) (angleOfLine line1) (angleOfLine line2)
    stroke

    do let fontSize = 10
       let Vec2 x y = point `addVec2` Vec2 15 15
       hsva 0 0 0 1
       moveTo x y
       setFontSize fontSize
       let Angle alpha = angle
           angleDeg = round (alpha / (2 * pi) * 360) :: Int
       showText (show ty ++ ", " ++ show angleDeg ++ "°")

    closePath
