module Test.Geometry.Algorithms.Contour (tests) where



import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C
import qualified System.Random.MWC        as MWC

import Draw
import Geometry as G
import Geometry.Algorithms.Contour.Internal

import Test.TastyAll



tests :: TestTree
tests = testGroup "Contour finding"
    [ findRootOnLineTests
    , optimizeDiscreteLineTests
    , applyThresholdTests
    , classifyTests
    , contourEdgesTests
    , closedIsoIsClosedTest
    , visualTests
    ]

findRootOnLineTests :: TestTree
findRootOnLineTests = testGroup "Narrow down root location on a line"
    [ testCase "Linear function along x axis" $ do
        let line = Line (Vec2 (-1) 0) (Vec2 1 0)
            f (Vec2 x _) = x
            actual = binarySearchRoot f line 1e-10
            expected = Vec2 0 0
        assertApproxEqual "" (ExpectedWithin 1e-10 expected) (Actual actual)
    , testCase "Polynomial function along diagonal" $ do
        let line = Line (Vec2 (-1) (-1)) (Vec2 1 1)
            f (Vec2 x y) = (x+1)*y^2
            actual = binarySearchRoot f line 1e-10
            expected = Vec2 0 0
        assertApproxEqual "" (ExpectedWithin 1e-10 expected) (Actual actual)
    ]

optimizeDiscreteLineTests :: TestTree
optimizeDiscreteLineTests = testGroup "Optimize discrete line" []

applyThresholdTests :: TestTree
applyThresholdTests = testGroup "Apply threshold" []

classifyTests :: TestTree
classifyTests = testGroup "Classify" []

contourEdgesTests :: TestTree
contourEdgesTests = testGroup "Contour edges" []

closedIsoIsClosedTest :: TestTree
closedIsoIsClosedTest = testCase "Closed iso line results in closed trajectory" $ do
    let iso = head (isoLines (Grid (Vec2 (-2) (-2), Vec2 2 2) (10, 10)) normSquare 1)
    assertBool "First and last entries are not the same" (head iso == last iso)

visualTests :: TestTree
visualTests = testGroup "Visual"
    [ testVisual "Single parabola" 100 100 "docs/iso_lines/parabola" $ \(w, h) -> do
        let gridDimension = (Vec2 (-10) (-10), Vec2 10 10)
            isos = Polyline <$> isoLines (Grid gridDimension (10, 10)) (\(Vec2 x y) -> y-0.1*x*x) 0
            fitToBox :: (HasBoundingBox geo, Transform geo) => geo -> geo
            fitToBox =
                G.transform (G.transformBoundingBox gridDimension (Vec2 (0+10) (0+10), Vec2 (w-10) (h-10)) def)
        cairoScope $ do
            setLineWidth 1
            for_ (fitToBox isos) sketch
            setColor (mathematica97 0)
            stroke

    ,  testVisual "Concentric circles" 100 100 "docs/iso_lines/circles" $ \(w, h) -> do
        cartesianCoordinateSystem def
        for_ (zip [1..] [1,2..20]) $ \(colorIndex, r) -> do
            let gridDimension = (Vec2 (-10) (-10), Vec2 10 10)
                gridResolution = (32, 32)
                isos = Polyline <$> isoLines (Grid gridDimension gridResolution) (\(Vec2 x y) -> x*x+y*y) (r*r)
                fitToBox :: (HasBoundingBox geo, Transform geo) => geo -> geo
                fitToBox =
                    G.transform (G.transformBoundingBox gridDimension (Vec2 0 0, Vec2 w h) def)
            cairoScope $ do
                setLineWidth 1
                for_ (fitToBox isos) sketch
                setColor (mathematica97 colorIndex)
                stroke

    , testVisual "Bubble iso lines" 320 220 "docs/iso_lines/potentials" $ \_ -> do
        let geometry =
                let coulombPotential q center = \v -> q / (0.001 + normSquare (v -. center))
                    randomCharges = runST $ do
                        gen <- MWC.initialize (V.fromList [2])
                        fs <- replicateM 16 $ do
                            x <- MWC.uniformRM (70, 330) gen
                            y <- MWC.uniformRM (70, 230) gen
                            let q = 1000
                                center = Vec2 x y
                            pure (coulombPotential q center)
                        pure (\v -> sum [f v | f <- fs])
                in randomCharges

            gridDimension = (Vec2 (-400) (-300), Vec2 400 300)
            resolutionFactor = 80
            gridResolution = (4*resolutionFactor, 3*resolutionFactor)
            grid = Grid gridDimension gridResolution

            isoLinesAtThreshold = isoLines grid geometry

        C.translate (-20) (-50)
        for_ (zip [0..] [2**fromIntegral n | n <- [1..7]]) $ \(colorIx, threshold) -> do
            let isos = isoLinesAtThreshold threshold
            cairoScope $ do
                setLineWidth 1
                setColor (mathematica97 colorIx `withOpacity` threshold)
                for_ isos $ \path -> sketch (Polyline (simplifyTrajectoryRdp 0.4 (V.fromList path)))
                stroke
    ]
