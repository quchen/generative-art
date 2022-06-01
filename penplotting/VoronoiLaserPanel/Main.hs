module Main (main) where



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
        margin = 20
        paperBB = boundingBox [zero, Vec2 w h]
        drawInsideBB = boundingBox [zero +. Vec2 margin margin, Vec2 w h -. Vec2 margin margin]

    let (seeds, cells) = geometry drawInsideBB
    let plotSettings = def
            { _canvasBoundingBox = Just paperBB
            , _previewDrawnShapesBoundingBox = True
            , _previewPenTravelColor = Nothing
            }
        plotCells = runPlot plotSettings { _feedrate = 1000, _previewPenColor = mathematica97 3 } $ do
            for_ cells plot
        plotSeeds = runPlot plotSettings { _feedrate = 250, _previewPenColor = black } $ do
            for_ seeds $ \seed -> plot (Circle seed 0.5)

    writeGCodeFile "out/voronoi-laser-panel-cells.g" plotCells
    writeGCodeFile "out/voronoi-laser-panel-seeds.g" plotSeeds

    D.render "out/voronoi-laser-panel.svg" (round w) (round h) $ do
        D.coordinateSystem (D.MathStandard_ZeroBottomLeft_XRight_YUp h)
        _plotPreview plotCells
        _plotPreview plotSeeds

geometry :: BoundingBox -> ([Vec2], [Polygon])
geometry bb =
    let points = runST $ do
            gen <- MWC.create
            gaussianDistributedPoints gen bb (32 *. mempty) 256
            -- poissonDisc gen PoissonDiscParams
            --     { _poissonShape  = bb
            --     , _poissonRadius = 15
            --     , _poissonK      = 3
            -- }

        delaunay =
              lloydRelaxation 3
            . bowyerWatson bb
            $ points
        voronoi = toVoronoi delaunay

        cells = [ growPolygon (-1) (_voronoiRegion cell) | cell <- _voronoiCells voronoi]
        centers = map _voronoiSeed (_voronoiCells voronoi)
    in (centers, cells)
