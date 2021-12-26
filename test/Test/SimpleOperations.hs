module Test.SimpleOperations (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (x, y)

import Draw
import Geometry

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Simple operations"
    [ testVisual
    , pointInPolygonRegression
    ]

testVisual :: TestTree
testVisual = testCase "Visual" $ renderAllFormats 320 400 "test/out/simple_operations" (do
    Cairo.translate 30 50 >> rotateLineTest
    Cairo.translate  0 80 >> perpendicularBisectorTest
    Cairo.translate  0 80 >> perpendicularLineThroughTest
    Cairo.translate  0 80 >> pointInPolygonTest
    )

rotateLineTest :: Render ()
rotateLineTest = do
    let initialLine = angledLine (Vec2 0 0) (rad 0) (Distance 75)
        rotated = iterate (rotateAround (Vec2 25 0) (deg 20)) initialLine

    setLineWidth 1
    for_ (zip [0..8] rotated) (\(i, line) -> do
        mmaColor i 1
        lineSketch line
        stroke )

    mmaColor 1 1
    setFontSize 12
    moveTo 90 20
    showText "Rotate line in 20° increments"

perpendicularBisectorTest :: Render ()
perpendicularBisectorTest = do
    let line = angledLine (Vec2 0 0) (deg 30) (Distance 50)
        bisector = perpendicularBisector line

    setLineWidth 1
    mmaColor 0 1
    lineSketch line
    stroke
    mmaColor 1 1
    lineSketch bisector
    stroke

    setFontSize 12
    moveTo 40 10
    showText "Perpendicular bisector"

perpendicularLineThroughTest :: Render ()
perpendicularLineThroughTest = do
    let line = angledLine (Vec2 0 0) (deg 30) (Distance 50)
        point = Vec2 20 30
        line' = perpendicularLineThrough point line

    setLineWidth 1
    mmaColor 0 1
    lineSketch line
    stroke
    circleSketch point (Distance 3)
    stroke
    mmaColor 1 1
    lineSketch line'
    stroke

    setFontSize 12
    moveTo 40 40
    showText "Perpendicular line through point"

pointInPolygonTest :: Render ()
pointInPolygonTest = do
    let square = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        points = [Vec2 x (0.25*x + 20) | x <- [-15, -5 .. 60] ]

    setLineWidth 1
    polygonSketch square
    mmaColor 0 1
    strokePreserve
    mmaColor 0 0.1
    fill

    mmaColor 1 1
    for_ points (\point -> do
        circleSketch point (Distance 3)
        if pointInPolygon point square
            then fill
            else stroke )

    setFontSize 12
    moveTo 60 20
    showText "Point in polygon?"

-- This nasty point was inside the polygon, because the line leading towards it
-- crossed the polygon in three places: once as a normal intersection, and twice
-- as a single intersection with a corner. This led to the false claim that the
-- point was inside the polygon.
pointInPolygonRegression :: TestTree
pointInPolygonRegression = testCase "Point in polygon regression" $ do
    let polygon = Polygon [Vec2 (-80) (-80), Vec2 (-60) 80, Vec2 (-60) (-60)]
        point = Vec2 40.0 (-60)
    assertBool "Point should be outside of the polygon" (not (pointInPolygon point polygon))
