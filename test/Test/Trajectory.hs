module Test.Trajectory (tests) where


import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (x,y)

import Draw
import Geometry as G

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "Trajactories"
    [ simplifyPathTest
    ]

simplifyPathTest :: TestTree
simplifyPathTest = testCase "Simplify path" (renderAllFormats 400 300 "docs/interpolation/3_simplify_path" simplifyPathTestRender)

simplifyPathTestRender :: Render ()
simplifyPathTestRender = do
    let graph = [Vec2 x (sin x / (0.5 * x)) | x <- [0.1, 0.2 .. 16]]
        graphBB = boundingBox graph
        fitToBox :: Transform geo => geo -> geo
        fitToBox = G.transform (transformBoundingBox graphBB (boundingBox (Vec2 10 10, Vec2 (400-10) (100-10))) FitAllIgnoreAspect)

    let plotPath points = cairoScope $ do
            setLineWidth 1
            newPath
            pathSketch points
            stroke
        plotPoints points = for_ points $ \p -> do
            circleSketch p (Distance 1)
            fillPreserve
            cairoScope $ do
                setSourceRGB 0 0 0
                setLineWidth 0.5
                stroke
        plotBezier segments = cairoScope $ do
            setLineWidth 1
            bezierCurveSketch segments
            stroke
        epsilons = [ Distance (2**(-e)) | e <- [10,9..1]]

    cairoScope $ do
        mmaColor 0 1
        plotPath (fitToBox graph)
        plotPoints (fitToBox graph)

    cairoScope $ do
        for_ epsilons $ \epsilon -> do
            Cairo.translate 0 20
            let simplified = simplifyTrajectory epsilon graph
                interpolatedAgain = bezierSmoothen simplified
            mmaColor 1 1
            plotBezier (fitToBox interpolatedAgain)
            plotPoints (fitToBox simplified)
