module Test.Uncategorized.ConvexHull (tests) where



import Control.Monad
import Control.Monad.ST
import Data.List
import System.Random.MWC.Distributions as MWC

import Geometry
import Geometry.Chaotic

import Test.TastyAll



tests :: TestTree
tests = testGroup "Convex hull"
    [ idempotencyTest
    , allPointsInHullTest
    , convexHullIsConvexTest
    , isNotSelfIntersecting
    ]

idempotencyTest :: TestTree
idempotencyTest = testProperty "Idempotency" (forAll gen prop)
  where
    gen = do
        seed <- arbitrary
        numPoints <- choose (10, 100)
        pure $ runST $ do
            g <- initializeMwc (seed :: Int)
            replicateM numPoints (Vec2 <$> standard g <*> standard g)
    prop points = let Polygon hull  = convexHull points
                      Polygon hull' = convexHull hull
                  in hull === hull'

allPointsInHullTest :: TestTree
allPointsInHullTest = testProperty "All points are inside the hull" $
    let gen = do
            n <- choose (10, 100)
            replicateM n $ do
                Gaussian v <- arbitrary
                pure v
    in forAll gen $ \points ->
        let hull@(Polygon hullPoints) = convexHull points
        in all (\p -> pointInPolygon p hull) (points \\ hullPoints)

convexHullIsConvexTest :: TestTree
convexHullIsConvexTest = testProperty "Convex hull is convex" $
    let gen = do
            n <- choose (10, 100)
            replicateM n $ do
                Gaussian v <- arbitrary
                pure v
    in forAll gen $ \points -> isConvex (convexHull points)

isNotSelfIntersecting :: TestTree
isNotSelfIntersecting = testProperty "Result is satisfies polygon invariants" $
    let gen = do
            n <- choose (10, 100)
            replicateM n $ do
                Gaussian v <- arbitrary
                pure v
    in forAll gen $ \points -> case validatePolygon (convexHull points) of
        Left err -> error (show err)
        Right _ -> ()
