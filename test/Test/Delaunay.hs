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
    test = renderAllFormats 220 220 "docs/voronoi/delaunay_random" $ do
        gen <- liftIO create
        randomPoints <- liftIO $ poissonDisc PoissonDisc { width = 200, height = 200, radius = 40, k = 4, gen = gen }
        let triangulation = bowyerWatson randomPoints
        Cairo.translate 10 10
        for_ (getPolygons triangulation) $ \poly@(Polygon ps) -> cairoScope $ do
            polygonSketch poly
            mmaColor 0 1
            Cairo.setLineJoin Cairo.LineJoinBevel
            Cairo.stroke
            mmaColor 1 1
            for_ ps $ \p -> do
                circleSketch p (Distance 4)
                Cairo.fill
