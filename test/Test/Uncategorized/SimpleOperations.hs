module Test.Uncategorized.SimpleOperations (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as C hiding (x, y)

import Draw
import Geometry

import Test.TastyAll



tests :: TestTree
tests = testGroup "Simple operations"
    [ testGroup "Visual"
        [ rotateLineTest
        , perpendicularBisectorTest
        , perpendicularLineThroughTest
        , pointInPolygonTest
        ]
    , pointInPolygonRegression
    ]

rotateLineTest :: TestTree
rotateLineTest = testVisual "Rotate line" 300 90 "docs/geometry/rotate_line" $ \_ -> do
    C.translate 30 30
    let initialLine = angledLine (Vec2 0 0) (rad 0) 75
        rotated = iterate (Geometry.transform (rotateAround (Vec2 25 0) (deg 20))) initialLine

    setLineWidth 1
    for_ (zip [0..8] rotated) (\(i, line) -> do
        setColor $ flare (i/8)
        lineSketch line
        stroke )

    setColor $ flare 0.5
    setFontSize 12
    moveTo 90 20
    showText "Rotate line in 20° increments"

perpendicularBisectorTest :: TestTree
perpendicularBisectorTest = testVisual "Perpendicular bisector" 190 60 "docs/geometry/perpendicular_bisector" $ \_ -> do
    C.translate 10 20
    let line = angledLine (Vec2 0 0) (deg 30) 50
        bisector = perpendicularBisector line

    setLineWidth 1
    setColor $ mathematica97 0
    lineSketch line
    stroke
    setColor $ mathematica97 1
    lineSketch bisector
    stroke

    setFontSize 12
    moveTo 40 10
    showText "Perpendicular bisector"

perpendicularLineThroughTest :: TestTree
perpendicularLineThroughTest = testVisual "Perpendicular line through point" 250 70 "docs/geometry/perpendicular_line_through_point" $ \_ -> do
    C.translate 10 10
    let line = angledLine (Vec2 0 0) (deg 30) 50
        point = Vec2 20 30
        line' = perpendicularLineThrough point line

    setLineWidth 1
    setColor $ mathematica97 0
    lineSketch line
    stroke
    circleSketch point 3
    stroke
    setColor $ mathematica97 1
    lineSketch line'
    stroke

    setFontSize 12
    moveTo 40 40
    showText "Perpendicular line through point"

pointInPolygonTest :: TestTree
pointInPolygonTest = testVisual "Point in polygon" 200 70 "docs/geometry/point_in_polygon" $ \_ -> do
    C.translate 30 10
    let square = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        points = [Vec2 x (0.25*x + 20) | x <- [-15, -5 .. 60] ]

    setLineWidth 1
    polygonSketch square
    setColor $ mathematica97 0
    strokePreserve
    setColor $ mathematica97 0 `withOpacity` 0.1
    fill

    setColor $ mathematica97 1
    for_ points (\point -> do
        circleSketch point 3
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
