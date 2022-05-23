module Main (main) where



import           Control.Monad
import           Control.Monad.ST
import qualified System.Random.MWC as MWC

import Draw                         as D
import Draw.Plotting
import Geometry                     as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Voronoi



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
        plotDelaunay = runPlot plotSettings { _previewPenColor = mathematica97 1 } $ do
            for_ delaunayPolygons plot
        plotVoronoi = runPlot plotSettings { _previewPenColor = mathematica97 0 } $ do
            for_ voronoiPolygons plot

    writeGCodeFile "out/delaunay-voronoi-delaunay.g" plotDelaunay
    writeGCodeFile "out/delaunay-voronoi-voronoi.g" plotVoronoi

    D.render "out/delaunay-voronoi.svg" (round w) (round h) $ do
        D.coordinateSystem (D.MathStandard_ZeroBottomLeft_XRight_YUp h)
        _plotPreview plotDelaunay
        _plotPreview plotVoronoi

geometry :: ([Polygon], [Polygon])
geometry =
    let calcBB = boundingBox [zero, Vec2 1000 1000]

        points = runST $ do
            gen <- MWC.create
            -- gaussianDistributedPoints gen calcBB (192 *. mempty) 192
            poissonDisc gen PoissonDiscParams
                { _poissonShape  = calcBB
                , _poissonRadius = 25
                , _poissonK      = 3
            }

        delaunay =
              lloydRelaxation 3
            . bowyerWatson calcBB
            . toList
            $ points
        voronoi = toVoronoi delaunay

        cutoffRadius = let (w,h) = boundingBoxSize calcBB
                       in min w h / 3

        delaunayPolygons = flip filter (getPolygons delaunay) $ \(Polygon corners) ->
            all (\corner -> any (\voronoiPolygon -> pointInPolygon corner voronoiPolygon) voronoiPolygons) corners

        voronoiPolygons = do
            cell <- _voronoiCells voronoi
            guard (norm (_voronoiSeed cell -. boundingBoxCenter calcBB) <= cutoffRadius)
            pure (_voronoiRegion cell)

    in (delaunayPolygons, voronoiPolygons)
