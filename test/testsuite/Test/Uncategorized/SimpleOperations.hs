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
        sketch line
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
    sketch line
    stroke
    setColor $ mathematica97 1
    sketch bisector
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
    sketch line
    stroke
    sketch (Circle point 3)
    stroke
    setColor $ mathematica97 1
    sketch line'
    stroke

    setFontSize 12
    moveTo 40 40
    showText "Perpendicular line through point"

-- This nasty point was inside the polygon, because the line leading towards it
-- crossed the polygon in three places: once as a normal intersection, and twice
-- as a single intersection with a corner. This led to the false claim that the
-- point was inside the polygon.
pointInPolygonRegression :: TestTree
pointInPolygonRegression = testCase "Point in polygon regression" $ do
    let polygon = Polygon [Vec2 (-80) (-80), Vec2 (-60) 80, Vec2 (-60) (-60)]
        point = Vec2 40.0 (-60)
    assertBool "Point should be outside of the polygon" (not (pointInPolygon point polygon))
