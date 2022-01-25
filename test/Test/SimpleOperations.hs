module Test.SimpleOperations (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as C hiding (x, y)

import Draw
import Geometry

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Simple operations"
    [ testGroup "Visual"
        [ rotateLineTest
        , perpendicularBisectorTest
        , perpendicularLineThroughTest
        , pointInPolygonTest
        , nonIntersectingRayTest
        ]
    , pointInPolygonRegression
    ]

rotateLineTest :: TestTree
rotateLineTest = testCase "Rotate line" $ renderAllFormats 300 90 "docs/geometry/rotate_line" $ do
    C.translate 30 30
    let initialLine = angledLine (Vec2 0 0) (rad 0) 75
        rotated = iterate (Geometry.transform (rotateAround (Vec2 25 0) (deg 20))) initialLine

    setLineWidth 1
    for_ (zip [0..8] rotated) (\(i, line) -> do
        setColor $ mmaColor i 1
        lineSketch line
        stroke )

    setColor $ mmaColor 1 1
    setFontSize 12
    moveTo 90 20
    showText "Rotate line in 20° increments"

perpendicularBisectorTest :: TestTree
perpendicularBisectorTest = testCase "Perpendicular bisector" $ renderAllFormats 190 60 "docs/geometry/perpendicular_bisector" $ do
    C.translate 10 20
    let line = angledLine (Vec2 0 0) (deg 30) 50
        bisector = perpendicularBisector line

    setLineWidth 1
    setColor $ mmaColor 0 1
    lineSketch line
    stroke
    setColor $ mmaColor 1 1
    lineSketch bisector
    stroke

    setFontSize 12
    moveTo 40 10
    showText "Perpendicular bisector"

perpendicularLineThroughTest :: TestTree
perpendicularLineThroughTest = testCase "Perpendicular line through point" $ renderAllFormats 250 70 "docs/geometry/perpendicular_line_through_point" $ do
    C.translate 10 10
    let line = angledLine (Vec2 0 0) (deg 30) 50
        point = Vec2 20 30
        line' = perpendicularLineThrough point line

    setLineWidth 1
    setColor $ mmaColor 0 1
    lineSketch line
    stroke
    circleSketch point 3
    stroke
    setColor $ mmaColor 1 1
    lineSketch line'
    stroke

    setFontSize 12
    moveTo 40 40
    showText "Perpendicular line through point"

pointInPolygonTest :: TestTree
pointInPolygonTest = testCase "Point in polygon" $ renderAllFormats 200 70 "docs/geometry/point_in_polygon" $ do
    C.translate 30 10
    let square = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        points = [Vec2 x (0.25*x + 20) | x <- [-15, -5 .. 60] ]

    setLineWidth 1
    polygonSketch square
    setColor $ mmaColor 0 1
    strokePreserve
    setColor $ mmaColor 0 0.1
    fill

    setColor $ mmaColor 1 1
    for_ points (\point -> do
        circleSketch point 3
        if pointInPolygon point square
            then fill
            else stroke )

    setFontSize 12
    moveTo 60 20
    showText "Point in polygon?"
    fill

nonIntersectingRayTest :: TestTree
nonIntersectingRayTest = testCase "Non-intersecting ray" $ renderAllFormats 170 120 "docs/geometry/non_intersecting_ray" $ do
    C.translate 30 50
    let paintRay r@(Line start _) = cairoScope $ do
            newPath
            setLineWidth 1
            setColor (mmaColor 0 1)
            circleSketch start 2
            fill
            arrowSketch r def
            stroke
        paintGeometry points = for_ points $ \vec2 -> do
            setLineWidth 1
            setColor (mmaColor 1 1)
            moveToVec vec2
            newPath
            circleSketch vec2 2
            stroke

    let geometry = [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        rays = do
            origin <-
                [ Vec2 (-5) 10
                -- , Vec2 25 25
                -- , Vec2 10 10
                -- , Vec2 75 25
                ]
            let ray = nonIntersectingRay origin geometry
            pure (resizeLine (const 50) ray)

    cairoScope $ grouped (paintWithAlpha 0.2) $ radialCoordinateSystem (Vec2 25 25) 100
    paintGeometry geometry
    for_ rays paintRay

-- This nasty point was inside the polygon, because the line leading towards it
-- crossed the polygon in three places: once as a normal intersection, and twice
-- as a single intersection with a corner. This led to the false claim that the
-- point was inside the polygon.
pointInPolygonRegression :: TestTree
pointInPolygonRegression = testCase "Point in polygon regression" $ do
    let polygon = Polygon [Vec2 (-80) (-80), Vec2 (-60) 80, Vec2 (-60) (-60)]
        point = Vec2 40.0 (-60)
    assertBool "Point should be outside of the polygon" (not (pointInPolygon point polygon))
