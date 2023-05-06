{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Geometry.Algorithms.Delaunay.Delaunator (tests) where



import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST
import           Data.Foldable
import           Data.Function
import           Data.Ord
import           Data.STRef
import           Control.DeepSeq
import           Data.Traversable
import           Data.Vector                             (Vector, (!))
import qualified Data.Vector                             as V
import qualified Data.Vector.Algorithms.Intro            as VM
import           Data.Vector.Mutable                     (STVector)
import qualified Data.Vector.Mutable                     as VM
import qualified Draw                                    as D
import           Geometry
import           Geometry.Algorithms.Delaunay.Delaunator as Delaunator
import           Geometry.Core
import qualified Graphics.Rendering.Cairo                as C
import qualified System.Random.MWC                       as MWC
import           Test.TastyAll



tests :: TestTree
tests = testGroup "Delaunator"
    [ test_orient
    , test_circum_x
    , test_inCircle
    , test_find_closest_point
    , test_triangulation_new
    , test_triangulation_len_after_new
    , test_triangulation_empty_after_new
    , test_find_seed_triangle
    , test_triangulate
    ]

test_orient :: TestTree
test_orient = testGroup "orient"
    [ testCase "Screen coordinates: counterclockwise => >0" $ do
        let a = Vec2 0 0
            b = Vec2 10 0
            p = Vec2 5 (-5) -- To the left of a---b
            actual = orient a b p
        assertBool "Clockwise: positive" (actual > 0)
    , testCase "Screen coordinates: clockwise => <0" $ do
        let a = Vec2 0 0
            b = Vec2 10 0
            p = Vec2 5 5 -- To the right of a---b
            actual = orient a b p
        assertBool "Couter-clockwise: negative" (actual < 0)
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
        let actual = Actual (circumdelta a b c)
            expected = ExpectedWithin 1e-10 (Vec2 50 50)
        assertApproxEqual "" expected actual
    , testCase "circumRadiusSquare" $ do
        let arbitraryOffset = Vec2 12 34
            actual = Actual $ circumradiusSquare (a+.arbitraryOffset) (b+.arbitraryOffset) (c+.arbitraryOffset)
            expected = ExpectedWithin 1e-10 (normSquare (Vec2 50 50))
        assertApproxEqual "circumradius²" expected actual
    , testCase "circumcenter" $ do
        let arbitraryOffset = Vec2 12 34
            actual = Actual $ circumcenter (a+.arbitraryOffset) (b+.arbitraryOffset) (c+.arbitraryOffset)
            expected = ExpectedWithin 1e-10 (Vec2 50 50 +. arbitraryOffset)
        assertApproxEqual "circumcenter" expected actual
    ]
  where
    [a,b,c] = niceTestTriangle

test_inCircle :: TestTree
test_inCircle = testGroup "inCircle"
    [ testCase "Inside" $ do
        let p = Vec2 1 1
            [a,b,c] = niceTestTriangle
            actual = Actual (inCircle p a b c)
            expected = Expected True
        assertEqual "Point should be inside" expected actual
    , testCase "Outside" $ do
        let p = Vec2 (-1) (-1)
            [a,b,c] = niceTestTriangle
            actual = Actual (inCircle p a b c)
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
        for_ [1..1000] $ \_ -> do
            point <- liftIO (MWC.uniformRM (Vec2 (-25) (-25), Vec2 125 125) gen)
            D.cairoScope $ do
                D.sketch (Circle point 1)
                D.setColor (D.mathematica97 (if inCircle point a b c then 2 else 3))
                C.fill
    ]

test_find_closest_point :: TestTree
test_find_closest_point = testGroup "Find closest point"
    [ testCase "Points on x axis" $ do
        let index = 10
            points = V.fromList [Vec2 (fromIntegral x) 0 | x <- [-10, -9 .. 10 :: Int]]
            p0 = points V.! index +. Vec2 0 10
            actual = Actual (find_closest_point points p0)
            expected = Expected (Just index)
        assertEqual "" expected actual
    , testCase "Needle’s index is not returned if points match exactly" $ do
        let index = 10
            points = V.fromList [Vec2 (fromIntegral x) 0 | x <- [-10, -9 .. 10 :: Int]]
            p0 = points V.! index

            actual = find_closest_point points p0
        assertBool "Expecting index not equal to the needle’s" (actual /= Just index)
    ]

test_triangulation_new :: TestTree
test_triangulation_new = testCase
    "triangulation_new does not crash"
    (triangulation_new 10 `seq` pure ())

test_triangulation_len_after_new :: TestTree
test_triangulation_len_after_new = testCase "triangulation_len reports size 0 after initialization" $ do
    let actual = Actual $ runST $ do
            tgl <- triangulation_new 123
            triangulation_len tgl
        expected = Expected 0 --
    assertEqual "" expected actual

test_triangulation_empty_after_new :: TestTree
test_triangulation_empty_after_new = testCase "Triangulation is reported empty after initialization" $ do
    let actual = Actual $ runST $ do
            tgl <- triangulation_new 123
            triangulation_is_empty tgl
        expected = Expected True
    assertEqual "" expected actual

test_find_seed_triangle :: TestTree
test_find_seed_triangle = testGroup "Find seed triangle"
    [ testCase "Smoke test" $ do
        let actual = find_seed_triangle points
            points = V.fromList niceTestTriangle
        actual `deepseq` pure ()
    , testCase "All points are distinct" $ do
        let firstTriangle = find_seed_triangle points
            points = V.fromList niceTestTriangle
        assertValidTriangle firstTriangle
    , testCase "Seed triangle of a point cloud" $ do
        let points = runST $ do
                gen <- MWC.initialize (V.fromList [152])
                ps <- replicateM 100 (MWC.uniformRM (Vec2 0 0, Vec2 1000 1000) gen)
                pure (V.fromList ps)
            firstTriangle = find_seed_triangle points
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
                tri = runST $ do
                    tglMut <- Delaunator.triangulate points
                    freezeTriangulation tglMut
            tri `deepseq` pure ()
        , triangulateSmoketest 3 [142]
        , triangulateSmoketest 4 [13]
        ]
    ]

triangulateSmoketest :: Int -> [Int] -> TestTree
triangulateSmoketest n seed = testCase ("Smoke test: " ++ show n ++ " random points") $ do
    let points = runST $ do
            gen <- MWC.initialize (V.fromList (map fromIntegral seed))
            ps <- replicateM n (MWC.uniformRM (Vec2 0 0, Vec2 1000 1000) gen)
            pure (V.fromList ps)
        tri = runST $ do
            tglMut <- Delaunator.triangulate points
            freezeTriangulation tglMut
    tri `deepseq` pure ()
