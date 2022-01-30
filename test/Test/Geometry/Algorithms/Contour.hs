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

import Test.Common
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck



tests :: TestTree
tests = testGroup "Contour finding"
    [ findRootOnLineTests
    , optimizeDiscreteLineTests
    , fromGridTests
    , valueTableTests
    , applyThresholdTests
    , classifyTests
    , contourEdgesTests
    , visualTests
    ]

findRootOnLineTests :: TestTree
findRootOnLineTests = testGroup "Narrow down root location on a line"
    [ testCase "Linear function along x axis" $ do
        let line = Line (Vec2 (-1) 0) (Vec2 1 0)
            f (Vec2 x _) = x
            actual = binarySearchRoot f line 1e-10
            expected = Vec2 0 0
        assertApproxEqual expected actual
    , testCase "Polynomial function along diagonal" $ do
        let line = Line (Vec2 (-1) (-1)) (Vec2 1 1)
            f (Vec2 x y) = (x+1)*y^2
            actual = binarySearchRoot f line 1e-10
            expected = Vec2 0 0
        assertApproxEqual expected actual
    ]
  where
    assertApproxEqual expected actual =
        let errMsg = unlines
                [ "Expected: " ++ show expected
                , "Actual:   " ++ show actual
                ]
        in assertBool errMsg (expected ~== actual)

optimizeDiscreteLineTests :: TestTree
optimizeDiscreteLineTests = testGroup "Optimize discrete line" []

fromGridTests :: TestTree
fromGridTests = testGroup "Convert grid coordinates to continuous"
    [ testGroup "Square continuous, square discrete"
        [testCase "Start" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (11, 11)
            assertBool "xxx" $ fromGrid grid (IVec2 0 0) ~== Vec2 0 0
        , testCase "Middle" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (11, 11)
            assertBool "xxx" $ fromGrid grid (IVec2 11 11) ~== Vec2 1 1
        , testCase "End" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (10, 10)
            assertBool "xxx" $ fromGrid grid (IVec2 5 5) ~== Vec2 0.5 0.5
        ]
    , testGroup "Square continuous, rectangular discrete"
        [testCase "Start" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (11, 9)
            assertBool "xxx" $ fromGrid grid (IVec2 0 0) ~== Vec2 0 0
        , testCase "Middle" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (11, 9)
            assertBool "xxx" $ fromGrid grid (IVec2 11 9) ~== Vec2 1 1
        , testCase "End" $ do
            let grid = Grid (Vec2 0 0, Vec2 1 1) (10, 8)
            assertBool "xxx" $ fromGrid grid (IVec2 5 4) ~== Vec2 0.5 0.5
        ]
    ]

valueTableTests :: TestTree
valueTableTests = testGroup "Value table creation"
    [ testProperty "Dimension of created table" $
        let gen = do
                iSize <- choose (0, 10)
                jSize <- choose (0, 10)
                let gridRange = (Vec2 0 0, Vec2 1 1)
                pure $ Grid gridRange (iSize, jSize)
        in forAll gen $ \grid@Grid{_numCells = (iSize, jSize)} ->
            let vt = valueTable grid (const ())
            in all (\v -> length v == jSize) vt
               &&
               length vt == iSize
    ]

applyThresholdTests :: TestTree
applyThresholdTests = testGroup "Apply threshold" []

classifyTests :: TestTree
classifyTests = testGroup "Classify" []

contourEdgesTests :: TestTree
contourEdgesTests = testGroup "Contour edges" []


visualTests :: TestTree
visualTests = testGroup "Visual"
    [ testCase "Single parabola" $ do
        renderAllFormats 100 100 "out/test_diagonal" $ do
            cairoScope $ grouped (paintWithAlpha 0.3) $ cartesianCoordinateSystem
            let gridDimension = (Vec2 (-10) (-10), Vec2 10 10)
                isos = isoLines (Grid gridDimension (10, 10)) (\(Vec2 x y) -> y-0.1*x*x) 0
                fitToBox :: (HasBoundingBox geo, Transform geo) => geo -> geo
                fitToBox =
                    G.transform (G.transformBoundingBox gridDimension (Vec2 (0+10) (0+10), Vec2 (100-10) (100-10)) FitAllMaintainAspect)
            cairoScope $ do
                setLineWidth 1
                for_ (fitToBox isos) pathSketch
                setColor (mmaColor 0 1)
                stroke

    ,  testCase "Concentric circles" $ do
        renderAllFormats 100 100 "out/test" $ do
            for_ (zip [1..] [1,3..9]) $ \(colorIndex, r) -> do
                let gridDimension = (Vec2 (-10) (-10), Vec2 10 10)
                    isos = isoLines (Grid gridDimension (30, 30)) (\(Vec2 x y) -> x*x+y*y) (r*r)
                    fitToBox :: (HasBoundingBox geo, Transform geo) => geo -> geo
                    fitToBox =
                        G.transform (G.transformBoundingBox gridDimension (Vec2 (0+10) (0+10), Vec2 (100-10) (100-10)) FitAllMaintainAspect)
                cairoScope $ do
                    setLineWidth 1
                    for_ (fitToBox isos) pathSketch
                    setColor (mmaColor colorIndex 1)
                    stroke

    , testCase "Bubble iso lines" $ do
        renderAllFormats 400 300 "out/test2" $ do
            let geometry =
                    let circle r center = \v -> r^2 / normSquare (v -. center)
                        randomParabolas = runST $ do
                            gen <- MWC.initialize (V.fromList [1])
                            fs <- replicateM 10 $ do
                                x <- MWC.uniformRM (0, 400) gen
                                y <- MWC.uniformRM (0, 300) gen
                                let r = 50
                                    center = Vec2 x y
                                pure (circle r center)
                            pure (\v -> sum [f v | f <- fs])
                    in randomParabolas

                gridDimension = (Vec2 (-400) (-300), Vec2 400 300)
                grid = let factor = 10 in Grid gridDimension (4*factor, 3*factor)

                isoLinesAtThreshold = isoLines grid geometry

            for_ (zip [0..] [1,2,3,4,5]) $ \(colorIx, threshold) -> do
                let isos = isoLinesAtThreshold threshold
                cairoScope $ do
                    setLineWidth 1
                    setColor (mmaColor colorIx threshold)
                    for_ isos (\path -> pathSketch path)
                    stroke
    ]
