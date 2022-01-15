{-# LANGUAGE RecordWildCards #-}
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
import Voronoi

tests :: TestTree
tests = testGroup "Delaunay triangulation"
    [ testRandomTriangulation
    , testConversionToVoronoi
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
