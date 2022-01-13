module Test.Bezier (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry as G

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Arc length parameterization"
    [ singleCurveTest
    ]

singleCurveTest :: TestTree
singleCurveTest = testCase "Single curve" (renderAllFormats 300 150 "docs/bezier/1_single_curve" (singleCurveRender 300 150))

singleCurveRender :: Int -> Int -> Render ()
singleCurveRender _w _h = do
    let curve = let curveRaw = G.transform (G.rotate (deg (-30))) (Bezier (Vec2 0 0) (Vec2 1 5) (Vec2 2.5 (-1)) (Vec2 3 3))
                    fitToBox = G.transform (transformBoundingBox curveRaw (Vec2 10 10, Vec2 290 90) FitAllIgnoreAspect)
                in fitToBox curveRaw
        evenlySpaced = bezierSubdivideS 16 curve
        unevenlySpaced = bezierSubdivideT 16 curve

        offsetBelow :: Transform geo => geo -> geo
        offsetBelow = G.transform (G.translate (Vec2 0 50))

    setLineWidth 1

    cairoScope $ do
        mmaColor 1 1
        bezierCurveSketch [curve]
        stroke
        bezierCurveSketch [offsetBelow curve]
        stroke

    for_ (zip evenlySpaced unevenlySpaced) $ \(e, u') -> do
        let u = offsetBelow u'
        let circle p = newPath >> circleSketch p (Distance 3) >> stroke
            connect p q = do
                let shrink factor = resizeLineSymmetric (\(Distance d) -> Distance (factor*d))
                    line = shrink 0.8 (Line p q)
                lineSketch line
                setDash [1,1] 0
                stroke
        cairoScope (mmaColor 0 1 >> circle e)
        cairoScope (mmaColor 3 1 >> circle u)
        cairoScope (setSourceRGBA 0 0 0 0.1 >> connect e u)
