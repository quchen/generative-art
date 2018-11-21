module Test.Visual.ConvexHull (tests) where



import Data.Foldable
import Data.List
import Graphics.Rendering.Cairo hiding (x,y)
import System.Random
import Data.Default.Class

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Visual.Common
import Test.Instances



tests :: TestTree
tests = testGroup "Convex hull"
    [ testCase "Visual" visualTest
    , idempotencyTest
    , allPointsInHullTest
    ]

newtype Seed = Seed Int

randomGaussianPoints :: Seed -> [Vec2]
randomGaussianPoints (Seed seed)
  = let go (u1:u2:rest)
            | u1 <= 0 = go rest -- to avoid diverging on log(0)
            | otherwise = let root1 = sqrt (-2 * log u1)
                              pi2u2 = 2 * pi * u2
                              x = root1 * cos pi2u2
                              y = root1 * sin pi2u2
                          in Vec2 x y : go rest
        go _ = error "Can’t happen, input is infinite"
    in go (randomRs (0, 1) (mkStdGen seed))

randomRadialPoints :: Seed -> [Vec2]
randomRadialPoints (Seed seed)
  = let angles = filter (< 360) (randomRs (0, 360) (mkStdGen seed))
        rs = randomRs (0, 100) (mkStdGen (seed+1))
        mkVec r angle = Vec2 (r * cos angle) (r * sin angle)
    in zipWith mkVec rs angles

visualTest :: IO ()
visualTest = do
    let points = (take 250 . filter (\vec -> norm vec <= Distance 100) . map (mulVec2 100))
                    (randomGaussianPoints (Seed 10))
        hull = convexHull points

    renderVisual points hull
    assertions points hull
  where
    renderVisual points hull = renderAllFormats 220 220 "test/out/convex_hull" $ do
        cartesianCoordinateSystem
        translate 110 110
        setLineWidth 1
        mmaColor 1 1
        for_ (polygonEdges hull) $ \edge -> do
            arrowSketch edge def{ arrowheadRelPos = Distance 0.5
                                , arrowheadSize = Distance 5 }
            stroke

        moveTo 0 100
        setFontSize 12
        showText "Convex hull"

        newPath
        mmaColor 0 1
        for_ points $ \p -> do
            circleSketch p (Distance 2)
            stroke

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
        pure (take numPoints (randomGaussianPoints (Seed seed)))
    prop points = let Polygon hull  = convexHull points
                      Polygon hull' = convexHull hull
                  in hull === hull'

newtype LotsOfGaussianPoints = LotsOfGaussianPoints [Vec2]
    deriving (Eq, Ord, Show)

instance Arbitrary LotsOfGaussianPoints where
    arbitrary = do
        Large seed <- arbitrary
        numPoints <- choose (10, 100)
        (pure . LotsOfGaussianPoints . take numPoints . randomGaussianPoints) (Seed seed)
    -- shrink (LotsOfGaussianPoints xs) = fmap LotsOfGaussianPoints (filter ((>= 3) . length) (shrink xs))
    -- TODO shrinking?

allPointsInHullTest :: TestTree
allPointsInHullTest = testProperty "All points are inside the hull" $
    \(LotsOfGaussianPoints points) ->
        let hull@(Polygon hullPoints) = convexHull points
        in all (\p -> pointInPolygon p hull) (points \\ hullPoints)
