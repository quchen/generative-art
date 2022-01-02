module Test.IntersectionLL (tests) where



import Graphics.Rendering.Cairo as Cairo hiding (translate, x, y)
import Text.Printf

import Draw
import Geometry

import Test.Common
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
        (angledLine (Vec2 50 190) (rad ( pi/6)) (Distance 100))
        (angledLine (Vec2 50 300) (rad (-pi/6)) (Distance 100))
    testVirtual2 = testDraw
        (translate (Vec2 30 380 +. polar (rad (-pi/6)) (Distance 20)) (angledLine (Vec2 0 0) (rad (-pi/6)) (Distance 100)))
        (translate (Vec2 30 380 +. polar (rad ( pi/6)) (Distance 20)) (angledLine (Vec2 0 0) (rad ( pi/6)) (Distance 100)))
    testVirtualInL = testDraw
        (angledLine (Vec2 300 180) (rad      0) (Distance 100))
        (angledLine (Vec2 350 200) (rad (pi/2)) (Distance  50))
    testVirtualR = testDraw
        (angledLine (Vec2 370 330) (rad      0) (Distance  50))
        (angledLine (Vec2 350 280) (rad (pi/2)) (Distance 100))
    testReal1 = testDraw
        (Line (Vec2 10  10) (Vec2 220 190))
        (Line (Vec2 270 50) (Vec2  30 160))
    testReal2 = testDraw
        (translate (Vec2 320 10) (Line (Vec2 0   0) (Vec2 120 120)))
        (translate (Vec2 320 10) (Line (Vec2 120 0) (Vec2 0   120)))

testDraw :: Line -> Line -> Render ()
testDraw line1 line2 = do
    let (point, ty) = intersectionLL line1 line2

    setLineWidth 1

    restoreStateAfter $ do
        mmaColor 0 1
        arrowSketch line1 def{arrowheadSize = Distance 8}
        stroke

    restoreStateAfter $ do
        mmaColor 1 1
        arrowSketch line2 def{arrowheadSize = Distance 8}
        stroke

    restoreStateAfter $ do
        mmaColor 3 1
        circleSketch point (Distance 3)
        fill

    restoreStateAfter $ do
        mmaColor 3 1
        angleSketch point (angleOfLine line1) (angleOfLine line2)
        stroke

    restoreStateAfter $ do
        let fontSize = 10
            Vec2 x y = point +. Vec2 15 15
            angleDeg = printf "%2.f" (getDeg (angleBetween line1 line2))
            tyStr = case ty of
                IntersectionVirtual        -> "Virtual"
                IntersectionVirtualInsideL -> "Virtual (but inside left argument)"
                IntersectionVirtualInsideR -> "Virtual (but inside right argument)"
                IntersectionReal           -> "Intersection"

        hsva 0 0 0 1
        moveTo x y
        setFontSize fontSize
        showText (tyStr ++ ", " ++ angleDeg ++ "°")
