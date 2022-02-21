{-# LANGUAGE RecordWildCards #-}
module Test.Geometry.Algorithms.Delaunay (tests) where



import Control.Monad.IO.Class
import Data.List (scanl')
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC (create)

import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Delaunay.Internal
import Draw
import Geometry
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Voronoi

import Test.TastyAll



tests :: TestTree
tests = testGroup "Delaunay triangulation"
    [ testRandomTriangulation
    , testConversionToVoronoi
    , testLloydRelaxation
    ]

testRandomTriangulation :: TestTree
testRandomTriangulation = testVisual "Random points" 220 220 "docs/voronoi/delaunay_random" $ \_ -> do
    triangulation <- liftIO $ randomDelaunay 200 200
    C.translate 10 10
    for_ (getPolygons triangulation) $ \poly@(Polygon ps) -> cairoScope $ do
        sketch poly
        setColor $ mathematica97 0
        C.setLineJoin C.LineJoinBevel
        C.stroke
        setColor $ mathematica97 1
        for_ ps $ \p -> do
            circleSketch p 4
            C.fill

testConversionToVoronoi :: TestTree
testConversionToVoronoi = testVisual "Conversion to Voronoi" 220 220 "docs/voronoi/delaunay_voronoi" $ \_ -> do
    triangulation <- liftIO $ randomDelaunay 200 200
    let voronoi = toVoronoi triangulation
    C.translate 10 10
    for_ (getPolygons triangulation) $ \poly@(Polygon ps) -> cairoScope $ do
        sketch poly
        setColor $ mathematica97 0 `withOpacity` 0.25
        C.setLineJoin C.LineJoinBevel
        C.stroke
        setColor $ mathematica97 1
        for_ ps $ \p -> do
            circleSketch p 4
            C.fill
    for_ (cells voronoi) $ \Cell{..} -> do
        setColor $ mathematica97 3
        sketch region
        C.stroke

randomDelaunay :: Int -> Int -> IO DelaunayTriangulation
randomDelaunay width height = do
    gen <- liftIO create
    randomPoints <- liftIO $ poissonDisc PoissonDisc { radius = fromIntegral (width * height) / 1000, k = 4, .. }
    pure $ bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 (fromIntegral width) (fromIntegral height))) randomPoints

testLloydRelaxation :: TestTree
testLloydRelaxation = testVisual "Lloyd relaxation" 850 220 "docs/voronoi/lloyd_relaxation" $ \_ -> do
    points <- liftIO $ do
        gen <- create
        uniformlyDistributedPoints gen 200 200 15
    let triangulation0 = bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 200 200)) (toList points)
        triangulations = scanl' (flip ($)) triangulation0 (replicate 3 lloydRelaxation)
    C.translate 10 10
    for_ triangulations $ \triangulation -> do
        for_ (cells (toVoronoi triangulation)) $ \Cell{..} -> cairoScope $ do
            setColor $ mathematica97 0
            sketch region
            C.stroke
            setColor $ mathematica97 3
            arrowSketch (Line seed (polygonCentroid region)) def { arrowheadSize = 4 }
            C.stroke
            setColor $ mathematica97 1
            circleSketch seed 4
            C.fill
        C.translate 210 0
