module Visual.Billard (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Visual.Common



tests :: TestTree
tests = testGroup "Billard process"
    [ testCase "Rectangular table" testRectangularTable
    , testCase "Rectangular table with hole" testHoleInTable
    , testCase "Lambda table" testLambdaTable
    ]

testRectangularTable :: IO ()
testRectangularTable = renderAllFormats 320 240 "test/out/billard_rectangular" (do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]
    billard (polygonEdges table) (Vec2 100 100) (deg 25) 16 )

testHoleInTable :: IO ()
testHoleInTable = renderAllFormats 320 240 "test/out/billard_rectangular_hole" (do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]
        hole = move (Vec2 80 80) (rotateAround (Vec2 0 0) (deg 31) (Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]))
    billard (polygonEdges table ++ polygonEdges hole) (Vec2 200 200) (deg 50) 64 )

testLambdaTable :: IO ()
testLambdaTable = renderAllFormats 332 360 "test/out/billard_lambda" (do
    let lambda = move (Vec2 10 10) (Polygon
            [ Vec2 0.387   340.156
            , Vec2 113.773 170.078
            , Vec2 0.387   0
            , Vec2 85.426  0
            , Vec2 312.195 340.156
            , Vec2 227.156 340.156
            , Vec2 156.293 233.859
            , Vec2 85.426  340.156
            , Vec2 0.387   340.156 ])
    billard (polygonEdges lambda) (Vec2 100 100) (deg 25) 256 )

billard :: [Line] -> Vec2 -> Angle -> Int -> Render ()
billard table startPoint startAngle numReflections = do
    let startVec = angledLine startPoint startAngle (Distance 100)
        billardPoints = startPoint : take numReflections (billardProcess table startVec)

    setLineWidth 1

    mmaColor 0 1
    setDash [2,4] 0
    for_ table (\edge -> lineSketch edge >> stroke)

    setDash [] 0
    mmaColor 0 1
    for_ billardPoints (\point -> do
        circleSketch point (Distance 3)
        stroke )
    mmaColor 1 1
    let billardArrows = zipWith Line billardPoints (tail billardPoints)
    for_ billardArrows (\arr -> do
        lineSketch arr
        stroke )
