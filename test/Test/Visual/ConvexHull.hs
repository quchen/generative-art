module Test.Visual.ConvexHull (tests) where



import Data.Default.Class
import Data.Foldable
import Data.List
import Data.Ord
import Graphics.Rendering.Cairo hiding (x, y)

import Draw
import Geometry

import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Visual.Common



tests :: TestTree
tests = testGroup "Convex hull"
    [ testCase "Visualll" visualTest
    , idempotencyTest
    , allPointsInHullTest
    , convexHullIsConvexTest
    , sameResultAsGrahamScan
    , isNotSelfIntersecting
    ]

visualTest :: IO ()
visualTest = do
    let points = (take 128 . filter (\vec -> norm vec <= Distance 100) . map (30 *.))
                    (gaussianVecs (Seed 55))
        hull = convexHull points

    renderVisual points hull
    assertions points hull
  where
    renderVisual points hull = renderAllFormats 210 180 "test/out/convex_hull" $ do
        translate 110 90
        setLineWidth 1
        for_ (polygonEdges hull) $ \edge@(Line start _) -> do
            mmaColor 1 1
            arrowSketch edge def{ arrowheadRelPos = Distance 0.5
                                , arrowheadSize   = Distance 5 }
            stroke
            mmaColor 3 1
            circleSketch start (Distance 2)
            fill

        mmaColor 1 1
        moveTo 0 85
        setFontSize 12
        showText "Convex hull"

        newPath
        mmaColor 0 0.5
        let Polygon hullPoints = hull
        for_ (points \\ hullPoints) $ \p -> do
            circleSketch p (Distance 2)
            fill

    assertions points hull =
        assertBool "Some points lie outside the hull!" $
            let notHullPoints = let Polygon hullPoints = hull in points \\ hullPoints
            in all (\p -> pointInPolygon p hull) notHullPoints

idempotencyTest :: TestTree
idempotencyTest = testProperty "Idempotency" (forAll gen prop)
  where
    gen = do
        Large seed <- arbitrary
        numPoints <- choose (10, 100)
        pure (take numPoints (gaussianVecs (Seed seed)))
    prop points = let Polygon hull  = convexHull points
                      Polygon hull' = convexHull hull
                  in hull === hull'

allPointsInHullTest :: TestTree
allPointsInHullTest = testProperty "All points are inside the hull" $
    \(LotsOfGaussianPoints points) ->
        let hull@(Polygon hullPoints) = convexHull points
        in all (\p -> pointInPolygon p hull) (points \\ hullPoints)

convexHullIsConvexTest :: TestTree
convexHullIsConvexTest = testProperty "Convex hull is convex" $
    \(LotsOfGaussianPoints points) -> isConvex (convexHull points)

sameResultAsGrahamScan :: TestTree
sameResultAsGrahamScan = testProperty "Equivalence to Graham Scan algorithm" $
    \(LotsOfGaussianPoints points) ->
        let actual@(Polygon actualPs) = normalizePolygon (convexHull points)
            reference@(Polygon referencePs) = normalizePolygon (convexHullGrahamScan points)
        in counterexample
            (unlines
                [ "Implementation does not match reference (Graham Scan)"
                , "Actual:    " ++ show actual
                , "Reference: " ++ show reference
                , "Points in actual, but not in reference:"
                , "    " ++ show (actualPs \\ referencePs)
                , "Points in reference, but not in actual:"
                , "    " ++ show (referencePs \\ actualPs)
                ])
            (actual == reference)

-- Graham Scan algorithm.
--
-- This algorithm uses angles to compute the complex hull, so it is unsuitable
-- for rational arithmetic, which is why we use Andrew’s algorithm in the
-- library instead.
convexHullGrahamScan :: [Vec2] -> Polygon
convexHullGrahamScan points
  = let pivot = minimumBy
            (\(Vec2 x1 y1) (Vec2 x2 y2) -> compare y1 y2 <> compare x1 x2)
            points
        -- TODO: make this compatible with rationals
        -- angleMonotonous p = angleBetween (Line pivot p) (Line (Vec2 0 0) (Vec2 1 0))
        angleMonotonous p@(Vec2 px py)
          =  dotProduct (p -. pivot) (Vec2 px (-py) -. pivot)
            / normSquare (p -. pivot)
        pointsSortedByXAngle = sortBy
            (comparing angleMonotonous <> comparing normSquare)
            points

        go [] (p:ps) = go [p] ps
        go [s] (p:ps) = go [p,s] ps
        go (s:t:ack) (p:ps)
          = let angleSign a b c = compare (det (b -. a) (c -. b)) 0
            in case angleSign t s p of
                LT -> go (p:s:t:ack)    ps
                EQ -> go (    t:ack) (p:ps) -- Ignore closer points with identical angle
                GT -> go (    t:ack) (p:ps)
        go stack [] = Polygon stack
    in go [] pointsSortedByXAngle

isNotSelfIntersecting :: TestTree
isNotSelfIntersecting = testProperty "Result is not self-intersecting" $
    \(LotsOfGaussianPoints points) -> null (selfIntersections (convexHull points))
