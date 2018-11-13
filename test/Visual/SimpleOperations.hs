module Visual.SimpleOperations (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo hiding (x, y)

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Visual.Common



tests :: TestTree
tests = testCase "Simple operations" testSimple

testSimple = renderAllFormats 320 240 "test/out/simple_operations" (do
    translate 10 20 >> perpendicularBisectorTest >> identityMatrix
    translate 10 90 >> perpendicularLineThroughTest >> identityMatrix
    translate 30 180 >> pointInPolygonTest >> identityMatrix
    )

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
    lineSketch (resizeLineSymmetric line' (const (Distance 50)))
    stroke

    setFontSize 12
    moveTo 40 40
    showText "Perpendicular line through point"

pointInPolygonTest :: Render ()
pointInPolygonTest = do
    let polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        points = [Vec2 x (0.25*x + 20) | x <- [-15, -5 .. 60] ]

    setLineWidth 1
    polygonSketch polygon
    mmaColor 0 1
    strokePreserve
    mmaColor 0 0.1
    fill

    mmaColor 1 1
    for_ points (\point -> do
        circleSketch point (Distance 3)
        if pointInPolygon point polygon
            then fill
            else stroke )

    setFontSize 12
    moveTo 60 20
    showText "Point in polygon?"
