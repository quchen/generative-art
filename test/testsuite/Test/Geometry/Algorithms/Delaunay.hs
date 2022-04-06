module Test.Geometry.Algorithms.Delaunay (tests) where



import           Control.Monad.IO.Class
import qualified Graphics.Rendering.Cairo as C
import qualified System.Random.MWC        as MWC

import           Draw                         hiding (Circle)
import qualified Draw                         as D
import           Geometry
import           Geometry.Algorithms.Delaunay
import           Geometry.Algorithms.Sampling
import           Geometry.Algorithms.Voronoi

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
            sketch (D.Circle p 4)
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
            sketch (D.Circle p 4)
            C.fill
    for_ (_voronoiCells voronoi) $ \VoronoiCell{..} -> do
        setColor $ mathematica97 3
        sketch _voronoiRegion
        C.stroke

randomDelaunay :: Int -> Int -> IO DelaunayTriangulation
randomDelaunay width height = do

    randomPoints <- liftIO $ do
        gen <- MWC.create
        poissonDisc gen PoissonDiscParams
            { _poissonRadius = fromIntegral (width * height) / 1000
            , _poissonK      = 4
            , _poissonShape = boundingBox [zero, Vec2 (fromIntegral width) (fromIntegral height)]
            }
    pure $ bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 (fromIntegral width) (fromIntegral height))) randomPoints

testLloydRelaxation :: TestTree
testLloydRelaxation = testVisual "Lloyd relaxation" 850 220 "docs/voronoi/lloyd_relaxation" $ \_ -> do
    points <- liftIO $ do
        gen <- MWC.create
        uniformlyDistributedPoints gen 200 200 15
    let triangulation0 = bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 200 200)) (toList points)
        triangulations = take 4 (iterate (lloydRelaxation 1) triangulation0)
    C.translate 10 10
    for_ triangulations $ \triangulation -> do
        for_ (_voronoiCells (toVoronoi triangulation)) $ \VoronoiCell{..} -> cairoScope $ do
            setColor $ mathematica97 0
            sketch _voronoiRegion
            C.stroke
            setColor $ mathematica97 3
            sketch (Arrow (Line _voronoiSeed (polygonCentroid _voronoiRegion)) def { _arrowheadSize = 4 })
            C.stroke
            setColor $ mathematica97 1
            sketch (D.Circle _voronoiSeed 4)
            C.fill
        C.translate 210 0
