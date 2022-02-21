module Test.Uncategorized.ConvexHull (tests) where



import           Control.Monad
import           Control.Monad.ST
import           Data.Default.Class
import           Data.Foldable
import           Data.Function
import           Data.List
import qualified Data.Vector                     as V
import           Graphics.Rendering.Cairo        as Cairo hiding (x, y)
import           System.Random.MWC               as MWC
import           System.Random.MWC.Distributions as MWC

import Draw
import Geometry
import Geometry.Chaotic

import Test.TastyAll



tests :: TestTree
tests = testGroup "Convex hull"
    [ idempotencyTest
    , allPointsInHullTest
    , convexHullIsConvexTest
    , isNotSelfIntersecting
    , visualTest
    ]

visualTest :: TestTree
visualTest = testVisual "Visual" 210 180 "docs/geometry/convex_hull" $ \_ -> do
    let points = runST $ do
            gen <- MWC.initialize (V.fromList [])
            replicateM 128 (fix $ \loop -> do
                v <- Vec2 <$> MWC.normal 0 30 gen <*> MWC.normal 0 30 gen
                if norm v <= 100 then pure v else loop)

        hull = convexHull points

    Cairo.translate 110 90
    setLineWidth 1
    for_ (polygonEdges hull) $ \edge@(Line start _) -> cairoScope $ do
        setColor $ mathematica97 1
        sketch (Arrow edge def{ arrowheadRelPos = 0.5
                              , arrowheadSize   = 5 })
        stroke
        setColor $ mathematica97 3
        sketch (Circle start 2)
        fill

    cairoScope $ do
        setColor $ mathematica97 1
        moveTo 0 85
        setFontSize 12
        showText "Convex hull"

    cairoScope $ do
        setColor $ mathematica97 0 `withOpacity` 0.5
        let Polygon hullPoints = hull
        for_ (points \\ hullPoints) $ \p -> do
            sketch (Circle p 2)
            fill

    liftIO $
        assertBool "Some points lie outside the hull!" $
            let notHullPoints = let Polygon hullPoints = hull in points \\ hullPoints
            in all (\p -> pointInPolygon p hull) notHullPoints

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
    in forAll gen $ \points -> isConvex (convexHull (points ))

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
