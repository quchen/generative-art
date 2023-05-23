module Test.Geometry.Algorithms.Delaunay.Internal.Delaunator.Api (tests) where



import           Control.Monad.ST
import           Data.Vector                  ((!))
import qualified Data.Vector                  as V
import           Geometry
import           Data.Foldable
import           Geometry.Algorithms.Sampling
import qualified System.Random.MWC            as MWC
import           Test.TastyAll

import Geometry.Algorithms.Delaunay.Internal.Delaunator.Api



tests :: TestTree
tests = testGroup "Delaunator"
    [ test_convexHull
    ]

test_convexHull :: TestTree
test_convexHull = testGroup "Convex hull [reversed]"
    [ testCase "Simple case: square" $ do
        let points = [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100]
            delaunay = delaunayTriangulation points
            reversePolygon (Polygon ps) = Polygon (reverse ps)
            actual = Actual (normalizePolygon . Polygon . toList . _convexHull $ delaunay)
            expected = Expected (normalizePolygon (reversePolygon (convexHull points)))
        assertEqual "Convex hull should match" expected actual
    , testCase "Convex hull matches the standard Core algorithm" $ do
        let n = 10
            seed = [1242]
            points = runST $ do
                gen <- MWC.initialize (V.fromList (map fromIntegral seed))
                gaussianDistributedPoints gen (boundingBox [zero, Vec2 1000 1000]) (100 *. mempty) n
            delaunay = delaunayTriangulation points
            reversePolygon (Polygon ps) = Polygon (reverse ps)
            actual = Actual (normalizePolygon . Polygon . toList . _convexHull $ delaunay)
            expected = Expected (normalizePolygon (reversePolygon (convexHull points)))
        assertEqual "Convex hull should match" expected actual
    , testCase "Hull orientation matches orientation of Delaunay triangles" $ do
        let n = 10
            seed = [1242]
            points = runST $ do
                gen <- MWC.initialize (V.fromList (map fromIntegral seed))
                gaussianDistributedPoints gen (boundingBox [zero, Vec2 1000 1000]) (100 *. mempty) n
            delaunay = delaunayTriangulation points
            actual = Actual (polygonOrientation . Polygon . toList . _convexHull $ delaunay)
            expected = Expected (polygonOrientation (_triangles delaunay ! 1))
        assertEqual "Convex hull should match" expected actual
    ]
