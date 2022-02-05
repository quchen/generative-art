module Test.Geometry.Processes.Billard (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry

import Test.TastyAll



tests :: TestTree
tests = testGroup "Billard process"
    [ rectangularTableTest
    , holeInTableTest
    , lambdaTableTest
    , brokenTableTest
    ]

rectangularTableTest, holeInTableTest, lambdaTableTest, brokenTableTest :: TestTree
rectangularTableTest = testVisual "Rectangular table"   320 240 "docs/billard/1_rectangular" (\_ -> rectangularTable)
holeInTableTest      = testVisual "Hole in table"       320 240 "docs/billard/2_hole"        (\_ -> holeInTable)
lambdaTableTest      = testVisual "Lambda-shaped table" 330 360 "docs/billard/3_lambda"      (\_ -> lambdaTable)
brokenTableTest      = testVisual "Broken table"        160 120 "docs/billard/4_broken"      (\_ -> brokenTable)

rectangularTable :: Render ()
rectangularTable = do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]
    renderBillard (polygonEdges table) (Vec2 100 100) (deg 25) 16

holeInTable :: Render ()
holeInTable = do
    let table = Polygon [Vec2 10 10, Vec2 310 10, Vec2 310 230, Vec2 10 230]
        hole = Geometry.transform (Geometry.translate (Vec2 80 80) <> rotateAround (Vec2 0 0) (deg 31)) (Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50])
    renderBillard (polygonEdges table ++ polygonEdges hole) (Vec2 200 200) (deg 50) 48

lambdaTable :: Render ()
lambdaTable = do
    let lambda = Geometry.transform (Geometry.translate (Vec2 10 10)) (Polygon
            [ Vec2 0.387   340.156
            , Vec2 113.773 170.078
            , Vec2 0.387   0
            , Vec2 85.426  0
            , Vec2 312.195 340.156
            , Vec2 227.156 340.156
            , Vec2 156.293 233.859
            , Vec2 85.426  340.156
            , Vec2 0.387   340.156 ])
    renderBillard (polygonEdges lambda) (Vec2 100 100) (deg 25) 128

brokenTable :: Render ()
brokenTable = do
    let tableVertices = [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
        table = zipWith Line tableVertices (tail tableVertices)
    Cairo.translate 50 10
    renderBillard table (Vec2 20 20) (deg 60) 1000

renderBillard :: [Line] -> Vec2 -> Angle -> Int -> Render ()
renderBillard table startPoint startAngle numReflections = do
    let startVec = angledLine startPoint startAngle 100
        billardPoints = startPoint : take numReflections (billard table startVec)

    setLineWidth 1

    cairoScope $ do
        setColor $ mathematica97 0
        setDash [2,4] 0
        for_ table (\edge -> lineSketch edge >> stroke)

    cairoScope $ do
        setColor $ mathematica97 0
        for_ billardPoints (\point -> do
            circleSketch point 3
            stroke )

    cairoScope $ do
        setColor $ mathematica97 1
        let billardArrows = zipWith Line billardPoints (tail billardPoints)
        for_ billardArrows (\arr -> do
            lineSketch arr
            stroke )
