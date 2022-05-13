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
import Graphics.Rendering.Cairo     as C



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

    let (delaunay, voronoi) = geometry

    let whenInCircle corners radius =
            let center = boundingBoxCenter canvasBB
            in when (all (\corner -> norm (corner -. center) <= radius) corners)

    let plotSettings = def
            { _canvasBoundingBox = Nothing
            , _previewPenTravelColor = Nothing
            , _previewDrawnShapesBoundingBox = False
            }
        plotDelaunay = runPlot plotSettings { _previewPenColor = mathematica97 1 } $ do
            for_ (getPolygons delaunay) $ \polygon -> do
                let Polygon corners = polygon
                plot polygon
        plotVoronoi = runPlot plotSettings { _previewPenColor = mathematica97 0 } $ do
            for_ (_voronoiCells voronoi) $ \cell -> do
                let polygon@(Polygon corners) = _voronoiRegion cell
                plot polygon

    writeGCodeFile "out/delaunay-voronoi-delaunay.g" plotDelaunay
    writeGCodeFile "out/delaunay-voronoi-voronoi.g" plotVoronoi

    do
        let trafo = transformBoundingBox geometryBigBB canvasBB def
        D.render "out/delaunay-voronoi.svg" (round w) (round h) $ do
            D.coordinateSystem (D.MathStandard_ZeroBottomLeft_XRight_YUp h)
            C.transform (D.toCairoMatrix trafo)
            cartesianCoordinateSystem def
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

geometry :: (DelaunayTriangulation, Voronoi ())
geometry =
    let points = runST $ do
            gen <- MWC.create
            poissonDisc gen PoissonDiscParams
                { _poissonShape = geometryBigBB
                , _poissonRadius = 50
                , _poissonK      = 5
                }
        delaunay =
            lloydRelaxation 5
            . bowyerWatson geometryBigBB
            . filter (\p -> norm (p -. boundingBoxCenter geometryBigBB) <= 450)
            $ points
        voronoi = toVoronoi delaunay
    in (delaunay, voronoi)
