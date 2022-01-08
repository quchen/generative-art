module Test.Billard (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Billard process"
    [ rectangularTableTest
    , holeInTableTest
    , lambdaTableTest
    , brokenTableTest
    ]

rectangularTableTest, holeInTableTest, lambdaTableTest, brokenTableTest :: TestTree
rectangularTableTest = testCase "Rectangular table"   (renderAllFormats 320 240 "docs/billard/1_rectangular" rectangularTable)
holeInTableTest      = testCase "Hole in table"       (renderAllFormats 320 240 "docs/billard/2_hole"        holeInTable)
lambdaTableTest      = testCase "Lambda-shaped table" (renderAllFormats 330 360 "docs/billard/3_lambda"      lambdaTable)
brokenTableTest      = testCase "Broken table"        (renderAllFormats 160 120 "docs/billard/4_broken"      brokenTable)

rectangularTable :: Render ()
rectangularTable = do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]
    billard (polygonEdges table) (Vec2 100 100) (deg 25) 16

holeInTable :: Render ()
holeInTable = do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]
        hole = Geometry.translate (Vec2 80 80) (rotateAround (Vec2 0 0) (deg 31) (Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]))
    billard (polygonEdges table ++ polygonEdges hole) (Vec2 200 200) (deg 50) 48

lambdaTable :: Render ()
lambdaTable = do
    let lambda = Geometry.translate (Vec2 10 10) (Polygon
            [ Vec2 0.387   340.156
            , Vec2 113.773 170.078
            , Vec2 0.387   0
            , Vec2 85.426  0
            , Vec2 312.195 340.156
            , Vec2 227.156 340.156
            , Vec2 156.293 233.859
            , Vec2 85.426  340.156
            , Vec2 0.387   340.156 ])
    billard (polygonEdges lambda) (Vec2 100 100) (deg 25) 128

brokenTable :: Render ()
brokenTable = do
    let tableVertices = [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
        table = zipWith Line tableVertices (tail tableVertices)
    Cairo.translate 50 10
    billard table (Vec2 20 20) (deg 60) 1000

billard :: [Line] -> Vec2 -> Angle -> Int -> Render ()
billard table startPoint startAngle numReflections = do
    let startVec = angledLine startPoint startAngle (Distance 100)
        billardPoints = startPoint : take numReflections (billardProcess table startVec)

    setLineWidth 1

    restoreStateAfter $ do
        mmaColor 0 1
        setDash [2,4] 0
        for_ table (\edge -> lineSketch edge >> stroke)

    restoreStateAfter $ do
        mmaColor 0 1
        for_ billardPoints (\point -> do
            circleSketch point (Distance 3)
            stroke )

    restoreStateAfter $ do
        mmaColor 1 1
        let billardArrows = zipWith Line billardPoints (tail billardPoints)
        for_ billardArrows (\arr -> do
            lineSketch arr
            stroke )
