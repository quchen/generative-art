module Main (main) where



import Data.Colour.Names
import qualified Data.Set as S
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as C

import Draw
import Draw.Plotting
import Geometry as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling.Vogel
import Data.List (sortOn)



picWidth, picHeight, squareSize :: (Num a, Ord a) => a
picWidth = 1440
picHeight = 1440
squareSize = min picWidth picHeight

center :: Vec2
center = Vec2 (picWidth/2) (picHeight/2)

plotSize :: Num a => a
plotSize = 440

main :: IO ()
main = do
    let extents = boundingBox [Vec2 0 0, Vec2 picWidth picHeight]

    let seeds = filter (`insideBoundingBox` extents) $ vogel VogelSamplingParams
            { _vogelRadius = squareSize / sqrt 2
            , _vogelCenter = center
            , _vogelDensity = 1282.10688 / squareSize^2
            }
        cutoff = 0.47 * squareSize
    
    print (length seeds)

    let delaunay = delaunayTriangulation seeds
        cells = V.toList $ clipCellsToBox extents $ voronoiCells delaunay
        _triangles = filter
            (\(Polygon ps) -> all (\p -> norm (p -. center) < cutoff) ps)
            (V.toList $ delaunayTriangles delaunay)
        edges = filter
            (\(Line a b) -> all (\p -> norm (p -. center) < cutoff) [a, b])
            (V.toList $ delaunayEdges delaunay)
        voronoi = filter
            (\(seed, _) -> norm (seed -. center) < cutoff)
            (zip seeds cells)
        addCellGutter (seed, poly) =
            let cellGutter = 2 + norm (center -. seed) / (12*12)
            in  (seed, shrinkPolygon cellGutter poly)
        voronoiSmoothed = fmap (snd . fmap (chaikin 0.25 . chaikin 0.25 . chaikin 0.15) . addCellGutter) voronoi

    render "out/vogel_sampling.png" picWidth picHeight $ drawPic edges voronoiSmoothed
    let plotResult = plotPic edges voronoiSmoothed
    renderPreview "out/vogel_sampling_preview.png" 5 plotResult

chaikin :: Double -> Polygon -> Polygon
chaikin _ (Polygon []) = Polygon []
chaikin lambda (Polygon ps@(p:_)) = Polygon $ concat
    [ [c, b]
    | (a, d) <- zip ps (tail ps ++ [p])
    , let b = lambda *. a +. (1-lambda) *. d
    , let c = (1-lambda) *. a +. lambda *. d
    ]

drawPic :: [Line] -> [Polygon] -> C.Render ()
drawPic delaunay voronoi = do
    cairoScope (setColor white >> paint)
    C.setLineJoin C.LineJoinBevel
    cairoScope $ do 
        setColor black
        setLineWidth 0.5
        for_ delaunay $ \poly -> do
            sketch poly
            C.stroke
    cairoScope $ do
        setColor black
        setLineWidth 1
        for_ voronoi $ \cell -> do
            sketch cell
            C.stroke

plotPic :: [Line] -> [Polygon] -> RunPlotResult
plotPic delaunay voronoi = runPlot plottingSettings $ do
    penChange
    for_ (resize mergedDelaunayEdges) plot
    penChange
    for_ (resize voronoi) plot
  where
    plottingSettings :: PlottingSettings
    plottingSettings = def
        { _feedrate = 6000
        , _zTravelHeight = 5
        , _zDrawingHeight = -2
        , _repositionThreshold = 0.05
        , _canvasBoundingBox = Just $ boundingBox [zero, Vec2 plotSize plotSize]
        , _previewPenWidth = 0.2
        }
    
    resize :: Transform a => a -> a
    resize = G.transform (G.scale (plotSize / squareSize))

    minimizePenHoveringSettings :: MinimizePenHoveringSettings Line
    minimizePenHoveringSettings = MinimizePenHoveringSettings
        { _getStartEndPoint = \(Line a b) -> (a, b)
        , _flipObject = Just (\(Line a b) -> Line b a)
        , _mergeObjects = Nothing
        }

    mergedDelaunayEdges :: [Line]
    mergedDelaunayEdges = minimizePenHoveringBy minimizePenHoveringSettings
        $ S.fromList
        $ sortOn (\(Line a b) -> getDeg $ angleBetween (Line center a) (Line a b))
        $ fmap (\(Line a b) -> if norm (center -. a) < norm (center -. b) then Line a b else Line b a)
        $ delaunay


penChange :: Plot ()
penChange = withDrawingHeight 0 $ do
    repositionTo zero
    penDown
    pause PauseUserConfirm
    penUp
