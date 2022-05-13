module Main (main) where



import           Control.Monad
import           Control.Monad.ST
import           Options.Applicative
import qualified System.Random.MWC   as MWC

import Draw                         as D
import Draw.Plotting
import Draw.Plotting.CmdArgs
import Geometry                     as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Voronoi
import Graphics.Rendering.Cairo     as C hiding (x,y)



main :: IO ()
main = do
    -- options <- commandLineOptions
    let options = CmdOptions
            { _canvas = Canvas
                {_canvasWidth = 594
                , _canvasHeight = 420
                , _canvasMargin = 0
                }
            }

    let Canvas { _canvasMargin = margin, _canvasWidth = w, _canvasHeight = h } = _canvas options
        canvasBB = boundingBox [zero +. Vec2 margin margin, Vec2 w h -. Vec2 margin margin]

    let (delaunayPolygons, voronoiPolygons) = geometry

    let whenInCircle corners radius =
            let center = boundingBoxCenter canvasBB
            in when (all (\corner -> norm (corner -. center) <= radius) corners)

    let plotSettings = def
            { _canvasBoundingBox = Nothing
            , _previewPenTravelColor = Nothing
            , _previewDrawnShapesBoundingBox = False
            }
        plotDelaunay = runPlot plotSettings { _previewPenColor = mathematica97 1 } $ do
            for_ delaunayPolygons plot
        plotVoronoi = runPlot plotSettings { _previewPenColor = mathematica97 0 } $ do
            for_ voronoiPolygons plot

    writeGCodeFile "out/delaunay-voronoi-delaunay.g" plotDelaunay
    writeGCodeFile "out/delaunay-voronoi-voronoi.g" plotVoronoi

    do
        let trafo = transformBoundingBox geometryBigBB canvasBB def
        D.render "out/delaunay-voronoi.svg" (round w) (round h) $ do
            D.coordinateSystem (D.MathStandard_ZeroBottomLeft_XRight_YUp h)
            C.transform (D.toCairoMatrix trafo)
            -- cartesianCoordinateSystem def
            _plotPreview plotDelaunay
            _plotPreview plotVoronoi

newtype CmdOptions = CmdOptions
    { _canvas :: Canvas
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO CmdOptions
commandLineOptions = execParser parserOpts
  where
    progOpts = CmdOptions
        <$> canvasP

    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Dual pen Delaunay-Voronoi circle." )

geometryBigBB :: BoundingBox
geometryBigBB = boundingBox [zero, Vec2 1000 1000]

geometry :: ([Polygon], [Polygon])
geometry =
    let points = runST $ do
            gen <- MWC.create
            gaussianDistributedPoints gen geometryBigBB (128 *. mempty) 256
        delaunay =
              lloydRelaxation 5
            . bowyerWatson geometryBigBB
            . toList
            $ points
        voronoi = toVoronoi delaunay

        cutoffRadius = let (w,h) = boundingBoxSize geometryBigBB
                       in min w h / 2
        isInside (Polygon corners) = all
            (\x -> norm (x -. boundingBoxCenter geometryBigBB) <= cutoffRadius)
            corners

        delaunayPolygons = filter isInside $ getPolygons delaunay
        voronoiPolygons = filter isInside $ map  _voronoiRegion (_voronoiCells voronoi)

    in (delaunayPolygons, voronoiPolygons)
