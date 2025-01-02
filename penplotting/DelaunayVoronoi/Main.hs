module Main (main) where



import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector as V
import qualified System.Random.MWC as MWC

import Draw                         as D
import Draw.Plotting
import Geometry                     as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling



main :: IO ()
main = do
    let w = 297
        h = 210
        margin = 10
        paperBB = boundingBox [zero, Vec2 w h]
        drawInsideBB = boundingBox [zero +. Vec2 margin margin, Vec2 w h -. Vec2 margin margin]

    let (delaunayPolygons, voronoiPolygons) = G.transform
            (transformBoundingBox (boundingBox geometry) drawInsideBB def)
            geometry

    let plotSettings = def
            { _canvasBoundingBox = Just paperBB
            , _previewDrawnShapesBoundingBox = True
            , _previewPenTravelColor = Nothing
            }
        plotDelaunay = runPlot plotSettings { _previewPenColor = mma 1 } $ do
            for_ delaunayPolygons plot
        plotVoronoi = runPlot plotSettings { _previewPenColor = mma 0 } $ do
            for_ voronoiPolygons plot

    writeGCodeFile "out/delaunay-voronoi-delaunay.g" plotDelaunay
    writeGCodeFile "out/delaunay-voronoi-voronoi.g" plotVoronoi

    D.render "out/delaunay-voronoi.svg" (round w) (round h) $ do
        D.coordinateSystem (D.MathStandard_ZeroBottomLeft_XRight_YUp h)
        _plotPreview plotDelaunay
        _plotPreview plotVoronoi

geometry :: (V.Vector Polygon, V.Vector Polygon)
geometry =
    let calcBB = boundingBox [zero, Vec2 1000 1000]

        points = V.fromList $ runST $ do
            gen <- MWC.create
            -- gaussianDistributedPoints gen calcBB (192 *. mempty) 192
            let poissonShape  = calcBB
                poissonRadius = 25
                poissonK      = 3
            poissonDisc gen poissonShape poissonRadius poissonK

        delaunay =
              delaunayTriangulation
            . V.last
            . V.iterateN 3 (lloydRelaxation calcBB 1)
            $ points

        cutoffRadius = let (w,h) = boundingBoxSize calcBB
                       in min w h / 3

        delaunayPolygons = flip V.filter (delaunayTriangles delaunay) $ \(Polygon corners) ->
            all (\corner -> norm (corner -. boundingBoxCenter calcBB) <= cutoffRadius) corners

        voronoiPolygons = do
            VoronoiFinite cell <- voronoiCells delaunay
            guard (any (\(Polygon corners) -> any (\corner -> pointInPolygon corner cell) corners) delaunayPolygons)
            pure cell

    in (delaunayPolygons, voronoiPolygons)
