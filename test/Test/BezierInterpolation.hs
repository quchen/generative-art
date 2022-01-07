module Test.BezierInterpolation (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo hiding (x,y)

import Draw
import Geometry

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Bezier interpolation"
    [ testGroup "Visual"
        [ somePoints
        ]
    ]

somePoints :: TestTree
somePoints = testCase "Some points" (renderAllFormats 400 300 "test/out/bezier_interpolation/1_some_points" rectangularTable)

rectangularTable :: Render ()
rectangularTable = paintBezierPicture points smoothed
  where
    points =
        Geometry.translate (Vec2 50 25) $ Geometry.scale 0.5 [ Vec2 100 500
        , Vec2 200 200
        , Vec2 500 500
        , Vec2 600 100
        , Vec2 400 50
        , Vec2 50 100
        , Vec2 50 50
        , Vec2 250 100
        , Vec2 200 500
        ]
    smoothed = bezierSmoothenOpen points

paintBezierPicture :: [Vec2] -> [Bezier Vec2] -> Render ()
paintBezierPicture points smoothed = do
    setLineWidth 1

    let circle r = restoreStateAfter $ do
            (x,y) <- getCurrentPoint
            newPath
            circleSketch (Vec2 x y) (Distance r)
            closePath
        prettyBezier (Bezier (Vec2 x0 y0) (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3) ) = do
            do -- Paint actual curve
                setSourceRGB 0 0 0
                moveTo x0 y0
                curveTo x1 y1 x2 y2 x3 y3
                stroke
            do -- paint bezier helper
                do -- forward
                    setSourceRGB 0.368417 0.506779 0.709798
                    moveTo x1 y1 >> circle 4 >> fill
                    moveTo x0 y0 >> lineTo x1 y1 >> stroke
                do -- backward
                    setSourceRGB 0.880722 0.611041 0.142051
                    moveTo x2 y2 >> circle 4 >> fill
                    moveTo x3 y3 >> lineTo x2 y2 >> stroke

        prettyPoint (Vec2 x y) = do
            save
            moveTo x y
            circle 5
            setSourceRGBA 0.922526 0.385626 0.209179 0.8
            fillPreserve
            setSourceRGB 0 0 0
            stroke
            restore

    for_ smoothed prettyBezier
    for_ points prettyPoint
