{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Geometry.Algorithms.Delaunay.Delaunator (tests) where



import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Ord
import           Data.STRef
import           Data.Traversable
import           Data.Vector                                (Vector, (!))
import qualified Data.Vector                                as V
import qualified Data.Vector.Algorithms.Intro               as VM
import           Data.Vector.Mutable                        (STVector)
import qualified Data.Vector.Mutable                        as VM
import qualified Draw                                       as D
import           Geometry
import qualified Geometry.Algorithms.Delaunay.Delaunator    as Delaunator
import qualified Geometry.Algorithms.Delaunay.DelaunatorApi as DelaunatorApi
import           Geometry.Algorithms.Sampling
import           Geometry.Core
import qualified Graphics.Rendering.Cairo                   as C
import qualified System.Random.MWC                          as MWC
import           Test.TastyAll



tests :: TestTree
tests = testGroup "Delaunator"
    [ test_orientation
    , test_circum_x
    , test_inCircleCcw
    , test_inCircle
    , test_find_closest_point
    , test_triangulation_new
    , test_find_seed_triangle
    , test_triangulate
    ]

screenClockwiseTriangle, screenCounterclockwiseTriangle :: (Vec2, Vec2, Vec2)
screenClockwiseTriangle = (Vec2 0 0, Vec2 100 0, Vec2 0 100)
screenCounterclockwiseTriangle = (Vec2 0 0, Vec2 0 100, Vec2 100 0)

test_orientation :: TestTree
test_orientation = testGroup "orientation"
    [ testCase "Screen coordinates: clockwise" $ do
        let (a,b,c) = screenClockwiseTriangle
            actual = Actual (Delaunator.orientation a b c)
            expected = Expected Delaunator.Clockwise
        assertEqual "Clockwise" expected actual
    , testCase "Screen coordinates: counterclockwise" $ do
        let (a,b,c) = screenCounterclockwiseTriangle
            actual = Actual (Delaunator.orientation a b c)
            expected = Expected Delaunator.Counterclockwise
        assertEqual "Clockwise" expected actual
    ]

niceTestTriangle :: [Vec2]
niceTestTriangle = [a,b,c]
  where
        --  (0,0)            (100,0)
        --  *-----------------*
        --  |             __/
        --  |          __/
        --  |       __/
        --  |    __/
        --  | __/
        --  |/
        --  * (0,100)
    a = Vec2 0 0
    b = Vec2 100 0
    c = Vec2 0 100

test_circum_x :: TestTree
test_circum_x = testGroup "Circum* functions"
    [ testCase "Circumdelta" $ do
        let actual = Actual (Delaunator.circumdelta a b c)
            expected = ExpectedWithin 1e-10 (Vec2 50 50)
        assertApproxEqual "" expected actual
    , testCase "circumRadiusSquare" $ do
        let arbitraryOffset = Vec2 12 34
            actual = Actual $ Delaunator.circumradiusSquare (a+.arbitraryOffset) (b+.arbitraryOffset) (c+.arbitraryOffset)
            expected = ExpectedWithin 1e-10 (normSquare (Vec2 50 50))
        assertApproxEqual "circumradius²" expected actual
    , testCase "circumcenter" $ do
        let arbitraryOffset = Vec2 12 34
            actual = Actual $ Delaunator.circumcenter (a+.arbitraryOffset) (b+.arbitraryOffset) (c+.arbitraryOffset)
            expected = ExpectedWithin 1e-10 (Vec2 50 50 +. arbitraryOffset)
        assertApproxEqual "circumcenter" expected actual
    ]
  where
    [a,b,c] = niceTestTriangle

test_inCircleCcw :: TestTree
test_inCircleCcw = testGroup "inCircleCcw"
    [ testCase "Sanitc check" $ do
        assertEqual "Screen counterclockwise triangle is not actually ccw!"
            (Expected Delaunator.Counterclockwise)
            (Actual (let (a,b,c) = screenCounterclockwiseTriangle in Delaunator.orientation a b c))
    , testCase "Inside" $ do
        let p = Vec2 1 1
            actual = Actual (Delaunator.inCircleCcw screenCounterclockwiseTriangle p)
            expected = Expected True
        assertEqual "Point should be inside" expected actual
    , testCase "Outside" $ do
        let p = Vec2 (-1) (-1)
            actual = Actual (Delaunator.inCircleCcw screenCounterclockwiseTriangle p)
            expected = Expected False
        assertEqual "Point should be outside" expected actual
    ]

test_inCircle :: TestTree
test_inCircle = testGroup "inCircle"
    [ testCase "Inside" $ do
        let p = Vec2 1 1
            actual = Actual (Delaunator.inCircle screenClockwiseTriangle p)
            expected = Expected True
        assertEqual "Point should be inside" expected actual
    , testCase "Outside" $ do
        let p = Vec2 (-1) (-1)
            actual = Actual (Delaunator.inCircle screenClockwiseTriangle p)
            expected = Expected False
        assertEqual "Point should be outside" expected actual
    , testVisual "Visual test" 150 150 "out/in_circle" $ \_ -> do
        C.translate 25 25
        let [a,b,c] = niceTestTriangle
        D.cairoScope $ do -- paint triangle
            C.setLineWidth 1
            D.sketch (Polygon [a,b,c])
            D.setColor (D.mathematica97 0)
            C.stroke
        gen <- liftIO MWC.create
        for_ [1..2^8] $ \_ -> do
            point <- liftIO (MWC.uniformRM (Vec2 (-25) (-25), Vec2 125 125) gen)
            D.cairoScope $ do
                C.setLineWidth 1
                if Delaunator.inCircle (a, b, c) point
                    then D.sketch (Circle point 1.5) >> D.setColor (D.mathematica97 2) >> C.fill
                    else D.sketch (D.Cross point 3) >>  D.setColor (D.mathematica97 3) >> C.stroke
    ]

test_find_closest_point :: TestTree
test_find_closest_point = testGroup "Find closest point"
    [ testCase "Points on x axis" $ do
        let index = 10
            points = V.fromList [Vec2 (fromIntegral x) 0 | x <- [-10, -9 .. 10 :: Int]]
            p0 = points V.! index +. Vec2 0 10
            actual = Actual (Delaunator.find_closest_point points p0)
            expected = Expected (Just index)
        assertEqual "" expected actual
    , testCase "Needle’s index is not returned if points match exactly" $ do
        let index = 10
            points = V.fromList [Vec2 (fromIntegral x) 0 | x <- [-10, -9 .. 10 :: Int]]
            p0 = points V.! index

            actual = Delaunator.find_closest_point points p0
        assertBool "Expecting index not equal to the needle’s" (actual /= Just index)
    ]

test_triangulation_new :: TestTree
test_triangulation_new = testCase
    "triangulation_new does not crash"
    (Delaunator.triangulation_new 10 `seq` pure ())

test_find_seed_triangle :: TestTree
test_find_seed_triangle = testGroup "Find seed triangle"
    [ testCase "Smoke test" $ do
        let actual = Delaunator.find_seed_triangle points
            points = V.fromList niceTestTriangle
        actual `deepseq` pure ()
    , testCase "All points are distinct" $ do
        let firstTriangle = Delaunator.find_seed_triangle points
            points = V.fromList niceTestTriangle
        assertValidTriangle firstTriangle
    , testCase "Seed triangle of a point cloud" $ do
        let points = runST $ do
                gen <- MWC.initialize (V.fromList [152])
                ps <- replicateM 100 (MWC.uniformRM (Vec2 0 0, Vec2 1000 1000) gen)
                pure (V.fromList ps)
            firstTriangle = Delaunator.find_seed_triangle points
        assertValidTriangle firstTriangle
    ]
  where
    assertValidTriangle Nothing = assertFailure "No initial triangle found"
    assertValidTriangle (Just abc@(a,b,c))
        | a == b || a == c || b == c = assertFailure ("Double corner in initial triangle: (a,b,c) = " <> show abc)
        | otherwise = pure ()

test_triangulate :: TestTree
test_triangulate = testGroup "Triangulate"
    [ testGroup "Smoke tests"
        [ testCase "Smoke test: 3 easy points" $ do
            let points = V.fromList niceTestTriangle
                tri = Delaunator.triangulate points
            tri `deepseq` pure ()
        , triangulateSmoketest 3 [142]
        , triangulateSmoketest 4 [13]
        , triangulateSmoketest 10 [13]
        , triangulateSmoketest 100 [13]
        , triangulateSmoketest 1000 [13]
        , testGroup "Visual"
            [ triangulateVisualSmoketest 100 [142]
            ]
        ]
    ]

triangulateSmoketest :: Int -> [Int] -> TestTree
triangulateSmoketest n seed = testCase ("Smoke test: " ++ show n ++ " random points") $ do
    let points = runST $ do
            gen <- MWC.initialize (V.fromList (map fromIntegral seed))
            ps <- replicateM n (MWC.uniformRM (Vec2 0 0, Vec2 1000 1000) gen)
            pure (V.fromList ps)
        tri = Delaunator.triangulate points
    tri `deepseq` pure ()

triangulateVisualSmoketest :: Int -> [Int] -> TestTree
triangulateVisualSmoketest n seed =
    testVisual
        ("Visual smoke test: " ++ show n ++ " random points")
        1000
        1000
        "out/smoketest/delaunator" $ \(w, h) -> do
            let points = runST $ do
                    gen <- MWC.initialize (V.fromList (map fromIntegral seed))
                    gaussianDistributedPoints gen (boundingBox [zero, Vec2 w h]) (100 *. mempty) n
                tri = Delaunator.triangulate points
                triangles = DelaunatorApi.triangles points tri
            D.cairoScope $ for_ points $ \point -> do
                D.setColor (D.mathematica97 0)
                D.sketch (Circle point 5)
                C.fill
            D.cairoScope $ for_ (zip [1..] triangles) $ \(i, triangle) -> do
                D.setColor (D.mathematica97 i)
                D.sketch triangle
                C.stroke
