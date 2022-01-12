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
singleCurveTest = testCase "Single curve" (renderAllFormats 320 240 "docs/arc_length_parameterization/1_single_curve" (singleCurveRender 320 240))

singleCurveRender :: Int -> Int -> Render ()
singleCurveRender w h = do
    let curveRaw = Bezier (Vec2 0 0) (Vec2 0.6 0.8) (Vec2 1.4 0.6) (Vec2 1 0)
        fitToBox = G.transform (transformBoundingBox curveRaw (Vec2 10 10, Vec2 (fromIntegral w-10) (fromIntegral h-10)) FitAllIgnoreAspect)
        curve = fitToBox curveRaw
        evenlySpaced = bezierSubdivideS 10 curve
        unevenlySpaced = bezierSubdivideT 10 curve

    setLineWidth 1
    cairoScope $ do
        mmaColor 1 1
        bezierCurveSketch [curve]
        stroke
    cairoScope $ do
        for_ (zip unevenlySpaced evenlySpaced) $ \(unevenly, evenly) -> do
            cairoScope $ do
                circleSketch evenly (Distance 3)
                mmaColor 0 1
                stroke
            cairoScope $ do
                mmaColor 2 0.1
                circleSketch unevenly (Distance 3)
                fillPreserve
                mmaColor 2 0.4
                stroke
    pure ()
