{-# OPTIONS_GHC -w #-} -- TODO Disable this!

module Test.Geometry.Algorithms.Delaunay.Delaunator (tests) where



import           Control.DeepSeq
import           Control.Monad
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
import  Draw
import           Geometry
import qualified Geometry.Algorithms.Delaunay.Delaunator    as Delaunator
import qualified Geometry.Algorithms.Delaunay.DelaunatorApi as DApi
import           Geometry.Algorithms.Sampling
import           Geometry.Core
import  Graphics.Rendering.Cairo as C
import           Numerics.Interpolation
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
    , test_visual_delaunay_voronoi
    -- , test_projectToViewport
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
        cairoScope $ do -- paint triangle
            setLineWidth 1
            sketch (Polygon [a,b,c])
            setColor (mathematica97 0)
            stroke
        gen <- liftIO MWC.create
        for_ [1..2^8] $ \_ -> do
            point <- liftIO (MWC.uniformRM (Vec2 (-25) (-25), Vec2 125 125) gen)
            cairoScope $ do
                setLineWidth 1
                if Delaunator.inCircle (a, b, c) point
                    then sketch (Circle point 1.5) >> setColor (mathematica97 2) >> fill
                    else sketch (Cross point 3) >>  setColor (mathematica97 3) >> stroke
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
        ]
    , testGroup "Convex hull [reversed]"
        [ testCase "Simple case: square" $ do
            let points = [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
                delaunay = DApi.delaunayTriangulation points
                reversePolygon (Polygon points) = Polygon (reverse points)
                actual = Actual (normalizePolygon (DApi._convexHull delaunay))
                expected = Expected (normalizePolygon (reversePolygon (convexHull points)))
            assertEqual "Convex hull should match" expected actual
        , testCase "Convex hull matches the standard Core algorithm" $ do
            let n = 10
                seed = [1242]
                points = runST $ do
                    gen <- MWC.initialize (V.fromList (map fromIntegral seed))
                    gaussianDistributedPoints gen (boundingBox [zero, Vec2 1000 1000]) (100 *. mempty) n
                delaunay = DApi.delaunayTriangulation points
                reversePolygon (Polygon points) = Polygon (reverse points)
                actual = Actual (normalizePolygon (DApi._convexHull delaunay))
                expected = Expected (normalizePolygon (reversePolygon (convexHull points)))
            assertEqual "Convex hull should match" expected actual
        , testCase "Hull orientation matches orientation of Delaunay triangles" $ do
            let n = 10
                seed = [1242]
                points = runST $ do
                    gen <- MWC.initialize (V.fromList (map fromIntegral seed))
                    gaussianDistributedPoints gen (boundingBox [zero, Vec2 1000 1000]) (100 *. mempty) n
                delaunay = DApi.delaunayTriangulation points
                actual = Actual (polygonOrientation (DApi._convexHull delaunay))
                expected = Expected (polygonOrientation (DApi._triangles delaunay ! 1))
            assertEqual "Convex hull should match" expected actual
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

test_visual_delaunay_voronoi :: TestTree
test_visual_delaunay_voronoi = testGroup ("Delaunay+Voronoi for " ++ show n ++ " points")
    [ triangulateVisualSmoketest
    , testVisualPaintOnlyEdges
    , test_voronoi_edges
    , test_voronoi_cells
    , test_delaunay_voronoi
    , test_hull
    ]

  where
    n = 2^6
    seed = [2]
    (width, height) = (600::Int, 400::Int)
    sigma = fromIntegral (min width height) / 5
    points = runST $ do
        gen <- MWC.initialize (V.fromList (map fromIntegral seed))
        let margin = 100
            bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
        -- gaussianDistributedPoints gen bb (sigma *. mempty) n
        uniformlyDistributedPoints gen bb n
    delaunay = DApi.delaunayTriangulation points

    triangulateVisualSmoketest =
        testVisual
            ("Visual smoke test: " ++ show n ++ " random points")
            width
            height
            "out/smoketest/delaunator" $ \_ -> do
                let triangles = DApi._triangles delaunay
                setLineWidth 1
                cairoScope $ V.iforM_ triangles $ \i triangle -> do
                    setColor (mathematica97 (i+1))
                    sketch (growPolygon (-1) triangle)
                    stroke

    testVisualPaintOnlyEdges =
        testVisual
            ("Paint only unique edges for " ++ show n ++ " random points")
            width
            height
            "out/smoketest/delaunator-edges" $ \_ -> do
                let edges = DApi._edges delaunay
                    numEdges = length edges
                setLineWidth 1
                cairoScope $ for_ (zip [0..] edges) $ \(i, edge) -> do
                    setColor (mako (lerpID (0, numEdges) (0,1) i))
                    sketch edge
                    stroke

    test_voronoi_edges = testVisual "Voronoi edges" width height "out/smoketest/voronoi-edges" $ \_ -> do
        let voronoiEdges = DApi._voronoiEdges delaunay
            numEdges = length voronoiEdges
        cairoScope $ for_ (zip [0..] voronoiEdges) $ \(i, vedge) -> do
            setLineWidth 1
            setColor (rocket (lerpID (0, numEdges) (0,1) i))
            sketch vedge
            stroke

    test_voronoi_cells = testVisual "Voronoi cells" width height "out/smoketest/voronoi-cells" $ \_ -> do
        setLineWidth 1
        for_ (DApi._edges delaunay) $ \edge -> cairoScope $ do
            setColor (mathematica97 0 `withOpacity` 0.2)
            sketch edge
            stroke
        for_ (DApi._voronoiCorners delaunay) $ \c -> cairoScope $ do
            sketch (Circle c 2)
            setColor (mathematica97 0 `withOpacity` 0.5)
            fill
        V.iforM_ (DApi._voronoiCells delaunay) $ \i (center, cell) -> cairoScope $ do
            let infiniteEdge p dir = resizeLine (const 90) (Line p (p +. dir))
            setColor (mathematica97 i)
            cairoScope $ do
                sketch (Circle center 1.5)
                stroke
                setColor (black `withOpacity` 1)
                sketch (Circle center 8)
                fill
                setColor (mathematica97 i)
                centeredText center (show i)
            case cell of
                DApi.VoronoiFinite fcell -> do
                    sketch (growPolygon (-2) fcell)
                    stroke
                DApi.VoronoiInfinite inDir points outDir -> do
                    setLineWidth 2
                    setLineCap LineCapRound
                    cairoScope $ do
                        setDash [4,4] 0
                        cairoScope $ do
                            let inPoint:_ = points
                            sketch (infiniteEdge inPoint inDir)
                            stroke
                        cairoScope $ do
                            let outPoint = last points
                            sketch (infiniteEdge outPoint outDir)
                            stroke
                    cairoScope $ for_ (zipWith Line points (tail points)) $ \line -> do
                        setDash [10,4] 0
                        sketch line
                        stroke

    test_delaunay_voronoi = testVisual "Delaunay+Voronoi" width height "out/smoketest/delaunay-voronoi-edges" $ \(w,h) -> do
        setLineWidth 1
        for_ (DApi._voronoiCells delaunay) $ \(_, cell) -> do
            case cell of
                DApi.VoronoiInfinite{} -> pure ()
                DApi.VoronoiFinite cell' -> do
                    setColor (mathematica97 0)
                    sketch (growPolygon (-1) cell')
                    stroke
        for_ (DApi._edges delaunay) $ \edge -> do
            setColor (mathematica97 1)
            sketch edge
            stroke
        for_ points $ \point -> do
            setColor (mathematica97 1)
            sketch (Circle point 2)
            fill

    test_hull = testVisual "Hull" width height "out/smoketest/delaunay-hull" $ \(w,h) -> do
        setLineWidth 1
        let arrowSpec = def{_arrowheadSize = 5}
        cairoScope $ for_ (DApi._edges delaunay) $ \edge -> do
            setColor (mathematica97 0 `withOpacity` 0.2)
            sketch edge
            stroke
        cairoScope $ do
            setColor (mathematica97 0)
            let Polygon hullPoints = DApi._convexHull delaunay
            for_ (zipWith Line hullPoints (tail (cycle hullPoints))) $ \line -> do
                sketch (Arrow line arrowSpec{_arrowheadRelPos = 0.5})
                stroke
            for_ hullPoints $ \p -> do
                sketch (Circle p 2)
                fill

        for_ (V.indexed points) $ \(i, p) -> do
            setColor (mathematica97 3)
            centeredText p (show i)

        cairoScope $ do
            setColor (mathematica97 1)
            for_ (V.indexed (DApi._extRays delaunay)) $ \(p, ray) -> case ray of
                Nothing -> pure ()
                Just (rayInDir, rayOutDir) -> for_ [rayInDir, rayOutDir] $ \dir -> do
                    let line = resizeLine (const 50) (Line (points!p) (points!p +. dir))
                        Line _ textPos = resizeLine (const 60) line
                    sketch line
                    stroke
                    centeredText textPos (show p)



centeredText v str = cairoScope $ do
    moveToVec v
    showTextAligned HCenter VCenter str

-- test_projectToViewport :: TestTree
-- test_projectToViewport = testVisual "projectToViewport" 200 300 "out/smoketest/projectToViewport" $ \(w,h) -> do
--     let squareBB = boundingBox [Vec2 10 10, Vec2 w (h/2) -. Vec2 10 10]
--         rays = [ DApi.Ray (boundingBoxCenter squareBB +. polar (deg d) 20) (polar (deg d) 1) | d <- takeWhile (<360) [0,16..]]
--         -- missingRays = [ DApi.Ray (Vec2 100 250 +. polar (deg d) 20) (polar (deg d) 1) | d <- [-60,-50..280]]
--         -- rays = hittingRays ++ missingRays
--         -- rays = [ DApi.Ray (Vec2 100 250 +. polar (deg d) 20) (polar (deg d) 1) | d <- [-10]]
--         drawRay (DApi.Ray start direction) = resizeLine (const (max w h)) (Line start (start +. direction))
--     setLineWidth 1
--     cairoScope $ do
--         sketch (boundingBoxPolygon squareBB)
--         setColor (mathematica97 0)
--         stroke
--     for_ (zip [1..] rays) $ \(i, ray) -> cairoScope $ do
--         case DApi.projectToViewport squareBB ray of
--             Nothing -> do
--                 setDash [] 0
--                 setColor (D.rgb 1 0 0)
--                 sketch (drawRay ray)
--                 stroke
--             Just p -> do
--                 setColor (mathematica97 i)
--                 cairoScope $ do
--                     setDash [2,4] 0
--                     sketch (drawRay ray)
--                     stroke
--                 cairoScope $ do
--                     sketch (Circle p 3)
--                     stroke
