module Main (main) where



import           Control.Monad.ST
import           Data.List
import           Data.Traversable
import qualified System.Random.MWC as MWC

import Draw                             as D
import Draw.Plotting
import Draw.Plotting.GCode
import Geometry                         as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.SimplexNoise
import Geometry.Algorithms.Voronoi
import Numerics.Interpolation



main :: IO ()
main = do
    let w = 297
        h = 210
        margin = 20
        paperBB = boundingBox [zero, Vec2 w h]
        drawInsideBB = boundingBox [zero +. Vec2 margin margin, Vec2 w h -. Vec2 margin margin]

    let cells = geometry drawInsideBB
        (outlines, hatchings) = unzip cells
    let plotSettings = def
            { _canvasBoundingBox = Just paperBB
            , _previewDrawnShapesBoundingBox = True
            , _previewPenTravelColor = Nothing
            , _previewPenWidth = 0.3
            }
        plotCells = runPlot plotSettings { _previewPenColor = mathematica97 3, _feedrate = 2000 } $ do
            for_ outlines $ \polygon -> do
                let plotEdges = map plot (polygonEdges polygon)
                    wait = gCode [G04_Dwell_ms 100] -- Avoid oscillations on sudden change of direction
                sequence_ (intersperse wait plotEdges)
        plotHatchings = runPlot plotSettings { _previewPenColor = black, _feedrate = 2000 } $ do
            for_ hatchings plot

    writeGCodeFile "out/voronoi-hatched-cells.g" plotCells
    writeGCodeFile "out/voronoi-hatched-hatching.g" plotHatchings

    D.render "out/voronoi-hatched.svg" (round w) (round h) $ do
        D.coordinateSystem (D.MathStandard_ZeroBottomLeft_XRight_YUp h)
        _plotPreview plotCells
        _plotPreview plotHatchings

geometry :: BoundingBox -> [(Polygon, [Line])]
geometry bb = runST $ do
    gen <- MWC.create
    points <- do
        let stddev = 32
            numPoints = 256
        gaussianDistributedPoints gen bb (stddev *. mempty) numPoints

    let voronoi = toVoronoi (lloydRelaxation 3 (delaunayTriangulation bb points))
        cells = [ growPolygon (-1) (_voronoiRegion cell) | cell <- _voronoiCells voronoi]

    hatched <- for cells $ \cell -> do
        angle <- fmap deg (MWC.uniformRM (0, 180) gen)
        spacing <- do
            let cellArea = polygonArea cell
                bbArea = polygonArea (boundingBoxPolygon bb)
            pure (lerp (0, sqrt bbArea) (0.2,15) (sqrt cellArea))
        pure (cell, hatch cell angle spacing)

    pure hatched
