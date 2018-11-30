module Test.ConvexHull (tests) where



import Data.Default.Class
import Data.Foldable
import Data.List
import Graphics.Rendering.Cairo hiding (x, y)

import Draw
import Geometry

import Test.Common
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck



tests :: TestTree
tests = testGroup "Convex hull"
    [ idempotencyTest
    , allPointsInHullTest
    , convexHullIsConvexTest
    , isNotSelfIntersecting
    , visualTest
    ]

visualTest :: TestTree
visualTest = testCase "Visual" $ do
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
        seed <- arbitrary
        numPoints <- choose (10, 100)
        pure (take numPoints (gaussianVecs seed))
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

isNotSelfIntersecting :: TestTree
isNotSelfIntersecting = testProperty "Result is not self-intersecting" $
    \(LotsOfGaussianPoints points) -> null (selfIntersections (convexHull points))
