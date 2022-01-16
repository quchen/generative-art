{-# LANGUAGE RecordWildCards #-}
module Test.Delaunay (tests) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.List (scanl')
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (create)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Common (renderAllFormats)

import Delaunay
import Delaunay.Internal
import Draw
import Geometry
import Sampling
import Voronoi

tests :: TestTree
tests = testGroup "Delaunay triangulation"
    [ testRandomTriangulation
    , testConversionToVoronoi
    , testLloydRelaxation
    ]

testRandomTriangulation :: TestTree
testRandomTriangulation = testCase "Random points" test
  where
    test = renderAllFormats 220 220 "docs/voronoi/delaunay_random" $ do
        triangulation <- liftIO $ randomDelaunay 200 200
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

testConversionToVoronoi :: TestTree
testConversionToVoronoi = testCase "Conversion to Voronoi" test
  where
    test = renderAllFormats 220 220 "docs/voronoi/delaunay_voronoi" $ do
        triangulation <- liftIO $ randomDelaunay 200 200
        let voronoi = toVoronoi triangulation
        Cairo.translate 10 10
        for_ (getPolygons triangulation) $ \poly@(Polygon ps) -> cairoScope $ do
            polygonSketch poly
            mmaColor 0 0.25
            Cairo.setLineJoin Cairo.LineJoinBevel
            Cairo.stroke
            mmaColor 1 1
            for_ ps $ \p -> do
                circleSketch p (Distance 4)
                Cairo.fill
        for_ (cells voronoi) $ \Cell{..} -> do
            mmaColor 3 1
            polygonSketch region
            Cairo.stroke

randomDelaunay :: Int -> Int -> IO DelaunayTriangulation
randomDelaunay width height = do
    gen <- liftIO create
    randomPoints <- liftIO $ poissonDisc PoissonDisc { radius = fromIntegral (width * height) / 1000, k = 4, .. }
    pure $ bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 (fromIntegral width) (fromIntegral height))) randomPoints

testLloydRelaxation :: TestTree
testLloydRelaxation = testCase "Lloyd relaxation" test
  where
    test = renderAllFormats 850 220 "docs/voronoi/lloyd_relaxation" $ do
        points <- liftIO $ do
            gen <- create
            uniformlyDistributedPoints gen 200 200 15
        let triangulation0 = bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 200 200)) points
            triangulations = scanl' (flip ($)) triangulation0 (replicate 3 lloydRelaxation)
        Cairo.translate 10 10
        for_ triangulations $ \triangulation -> do
            for_ (cells (toVoronoi triangulation)) $ \Cell{..} -> cairoScope $ do
                mmaColor 0 1
                polygonSketch region
                Cairo.stroke
                mmaColor 3 1
                arrowSketch (Line seed (centroid region)) def { arrowheadSize = Distance 4 }
                Cairo.stroke
                mmaColor 1 1
                circleSketch seed (Distance 4)
                Cairo.fill
            Cairo.translate 210 0
