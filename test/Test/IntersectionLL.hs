module Test.IntersectionLL (tests) where



import Graphics.Rendering.Cairo as Cairo hiding (translate, x, y)
import Text.Printf

import Draw
import Geometry

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Line-line intersection"
    [ testCase "Real intersection" intersectionReal
    , testCase "Half-virtual intersection" intersectionHalfVirtual
    , testCase "Virtual intersection" intersectionVirtual
    , testCase "Parallel" intersectionParallel
    , testCase "Parallel lines that fail to be recognized as parallel" intersectionAlmostParallel
    ]

intersectionReal :: IO ()
intersectionReal = renderAllFormats 580 200 "docs/geometry/intersection/real" $ do
    testReal1
    testReal2
  where
    testReal1 = testDraw
        (Line (Vec2 10  10) (Vec2 220 190))
        (Line (Vec2 270 50) (Vec2  30 160))
    testReal2 = testDraw
        (translate (Vec2 320 10) (Line (Vec2 0   0) (Vec2 120 120)))
        (translate (Vec2 320 10) (Line (Vec2 120 0) (Vec2 0   120)))

intersectionHalfVirtual :: IO ()
intersectionHalfVirtual = renderAllFormats 580 120 "docs/geometry/intersection/half_virtual" $ do
    testVirtualInL
    testVirtualR
  where
    testVirtualInL = testDraw
        (angledLine (Vec2 10 10) (rad      0) (Distance 100))
        (angledLine (Vec2 60 30) (rad (pi/2)) (Distance  50))
    testVirtualR = testDraw
        (angledLine (Vec2 370 60) (rad      0) (Distance  50))
        (angledLine (Vec2 350 10) (rad (pi/2)) (Distance 100))

intersectionVirtual :: IO ()
intersectionVirtual = renderAllFormats 580 120 "docs/geometry/intersection/virtual" $ do
    testVirtual1
    testVirtual2
  where
    testVirtual1 = testDraw
        (angledLine (Vec2 50 0) (rad ( pi/6)) (Distance 100))
        (angledLine (Vec2 50 120) (rad (-pi/6)) (Distance 100))
    testVirtual2 = testDraw
        (translate (Vec2 300 60 +. polar (rad (-pi/6)) (Distance 20)) (angledLine (Vec2 0 0) (rad (-pi/6)) (Distance 100)))
        (translate (Vec2 300 60 +. polar (rad ( pi/6)) (Distance 20)) (angledLine (Vec2 0 0) (rad ( pi/6)) (Distance 100)))

intersectionParallel :: IO ()
intersectionParallel = renderAllFormats 580 120 "docs/geometry/intersection/parallel" $ do
    testParallel1
    testParallel2
  where
    testParallel1 = testDraw
        (Line (Vec2 50 40) (Vec2 150 40))
        (Line (Vec2 50 80) (Vec2 150 80))
    testParallel2 = testDraw
        (Line (Vec2 300 60) (Vec2 400 60))
        (Line (Vec2 420 60) (Vec2 520 60))

intersectionAlmostParallel :: IO ()
intersectionAlmostParallel = renderAllFormats 580 120 "docs/geometry/intersection/almost_parallel" $ testDraw
    (angledLine (Vec2 50 0) (rad (pi/6)) (Distance 80))
    (angledLine (Vec2 50 20) (rad (pi/6)) (Distance 80))

testDraw :: Line -> Line -> Render ()
testDraw line1@(Line start _) line2 = do
    setLineWidth 1

    restoreStateAfter $ do
        mmaColor 0 1
        arrowSketch line1 def{arrowheadSize = Distance 8}
        stroke

    restoreStateAfter $ do
        mmaColor 1 1
        arrowSketch line2 def{arrowheadSize = Distance 8}
        stroke

    case intersectionLL line1 line2 of
        Nothing -> restoreStateAfter $ do
            let Vec2 x y = start +. Vec2 15 15
            hsva 0 0 0 1
            moveTo x y
            setFontSize 10
            showText "No intersection"

        Just (point, ty) -> do

            restoreStateAfter $ do
                mmaColor 3 1
                circleSketch point (Distance 3)
                fill

            restoreStateAfter $ do
                mmaColor 3 1
                angleSketch point (angleOfLine line1) (angleOfLine line2)
                stroke

            restoreStateAfter $ do
                let Vec2 x y = point +. Vec2 15 15
                    angleDeg = printf "%2.f" (getDeg (angleBetween line1 line2))
                    tyStr = case ty of
                        IntersectionVirtual        -> "Virtual"
                        IntersectionVirtualInsideL -> "Virtual (but inside left argument)"
                        IntersectionVirtualInsideR -> "Virtual (but inside right argument)"
                        IntersectionReal           -> "Intersection"

                hsva 0 0 0 1
                moveTo x y
                setFontSize 10
                showText (tyStr ++ ", " ++ angleDeg ++ "°")
