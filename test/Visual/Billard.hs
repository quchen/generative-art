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
    , testCase "Lambda table" testLambdaTable
    ]

testRectangularTable :: IO ()
testRectangularTable = renderAllFormats 320 240 "test/out/billard_rectangular" (do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]
    billard table (Vec2 100 100) (deg 25) 16 )

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
    billard lambda (Vec2 100 100) (deg 25) 256 )

billard :: Polygon -> Vec2 -> Angle -> Int -> Render ()
billard table startPoint startAngle numReflections = do
    let startVec = angledLine startPoint startAngle (Distance 100)
        billardPoints = startPoint : take numReflections (billardProcess table startVec)

    setLineWidth 2
    hsva 0 0 0 0.3
    polygonSketch table
    stroke

    setLineWidth 1
    for_ billardPoints (\point -> do
        circleSketch point (Distance 3)
        hsva 300 1 0.7 0.3
        fill )
    hsva 300 1 0.7 0.5
    let billardArrows = zipWith Line billardPoints (tail billardPoints)
    for_ billardArrows (\arr -> do
        lineSketch arr
        stroke )
