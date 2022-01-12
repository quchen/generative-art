module Test.ArcLengthParameterization (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry as G
import Geometry.ArcLengthParameterization

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Arc length parameterization"
    [ singleCurveTest
    ]

singleCurveTest :: TestTree
singleCurveTest = testCase "Single curve" (renderAllFormats 320 170 "docs/arc_length_parameterization/1_single_curve" (singleCurveRender 320 170))

singleCurveRender :: Int -> Int -> Render ()
singleCurveRender _w _h = do
    let curve = let curveRaw = G.transform (G.rotate (deg (-30))) (Bezier (Vec2 0 0) (Vec2 1 5) (Vec2 2.5 (-1)) (Vec2 3 3))
                    fitToBox = G.transform (transformBoundingBox curveRaw (Vec2 0 0, Vec2 300 100) FitAllMaintainAspect )
                in fitToBox curveRaw
        evenlySpaced = bezierSubdivideS 16 curve
        unevenlySpaced = bezierSubdivideT 16 curve

    setLineWidth 1

    Cairo.translate 10 10
    cairoScope $ do
        mmaColor 1 1
        bezierCurveSketch [curve]
        stroke
    cairoScope $ for_ (zip [1..] evenlySpaced) $ \(i, p) -> do
        newPath
        circleSketch p (Distance 3)
        mmaColor 0 1
        stroke
        cairoScope $ do
            moveToVec (p +. Vec2 0 10)
            setFontSize 6
            showText (show i)

    Cairo.translate 0 50

    cairoScope $ do
        mmaColor 1 1
        bezierCurveSketch [curve]
        stroke
    cairoScope $ for_ (zip [1..] unevenlySpaced) $ \(i, p) -> do
        newPath
        circleSketch p (Distance 3)
        mmaColor 3 1
        stroke
        cairoScope $ do
            moveToVec (p +. Vec2 0 10)
            setFontSize 6
            showText (show i)
