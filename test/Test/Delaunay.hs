module Test.Delaunay (tests) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (create)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Common (renderAllFormats)

import Delaunay
import Draw
import Geometry
import Sampling

tests :: TestTree
tests = testGroup "Delaunay triangulation"
    [ testRandomTriangulation
    ]

testRandomTriangulation :: TestTree
testRandomTriangulation = testCase "Random points" test
  where
    test = renderAllFormats 120 120 "docs/voronoi/delaunay_random" $ do
        gen <- liftIO create
        randomPoints <- liftIO $ uniformlyDistributedPoints gen 100 100 10
        let triangulation = bowyerWatson randomPoints
        for_ (getPolygons triangulation) $ \poly@(Polygon ps) -> cairoScope $ do
            polygonSketch poly
            mmaColor 0 1
            Cairo.stroke
            for_ ps $ \p -> do
                circleSketch p (Distance 2)
                mmaColor 1 1
                Cairo.fill
