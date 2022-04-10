module Main (main) where



import           Control.Monad
import           Control.Monad.ST
import           Data.Default.Class
import           Data.Foldable
import           Data.List
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as M
import qualified Data.Text.Lazy.IO               as TL
import qualified Data.Vector                     as V
import           Graphics.Rendering.Cairo        as C hiding
    (height, width, x, y)
import           Options.Applicative
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWC

import           Draw
import           Draw.Plotting
import           Draw.Plotting.CmdArgs
import           Geometry                         as G
import           Geometry.Algorithms.SimplexNoise
import           Geometry.Coordinates.Hexagonal   (Hex)
import qualified Geometry.Coordinates.Hexagonal   as Hex
import           Numerics.Interpolation



-- stack run gaussian-hexagons -- --a2 --margin 20 --landscape --samples $((2**18)) --cell-size 2 --stddev 45
main :: IO ()
main = do
    options <- commandLineOptions
    let polygons = hexPolygons options (gaussianHexagons options)
        thresholds = [-0.3, 0.3]
        [polygons1, _polygons2, polygons3] = partitionByThresholds thresholds polygons
    outputSvg options (polygons1, polygons3)
    outputGcode (polygons1, polygons3)

outputSvg :: Options -> (Map Hex (Polygon, Double), Map Hex (Polygon, Double)) -> IO ()
outputSvg options polygons = do
    let Options{_canvas=Canvas{_canvasWidth=imageWidth, _canvasHeight=imageHeight}} = options
    render "out/hexagons.svg" (round imageWidth) (round imageHeight) (drawHexagonsCairo polygons)

outputGcode :: (Map Hex (Polygon, Double), Map Hex (Polygon, Double)) -> IO ()
outputGcode (polygons1, polygons2) = do
    let plotterSettings = PlottingSettings
            { _feedrate          = Just 1000
            , _zTravelHeight     = 2
            , _zDrawingHeight    = -2
            , _zLoweringFeedrate = Just 500
            , _finishMove        = Just FinishWithG28
            , _previewDrawnShapesBoundingBox = True
            , _canvasBoundingBox = Nothing
            }

    TL.writeFile "out/hexagons-black.g" (runPlot plotterSettings (plotHexagons polygons1))
    TL.writeFile "out/hexagons-red.g" (runPlot plotterSettings (plotHexagons polygons2))

drawHexagonsCairo :: (Map Hex (Polygon, Double), Map Hex (Polygon, Double)) -> Render ()
drawHexagonsCairo (hexes1, hexes2) = cairoScope $ do
    cartesianCoordinateSystem def
    setLineWidth 1
    setColor black
    for_ hexes1 $ \(poly, _val) -> sketch poly
    stroke
    setColor (mathematica97 3)
    for_ hexes2 $ \(poly, _val) -> sketch poly
    stroke

plotHexagons :: Map Hex (Polygon, Double) -> Plot ()
plotHexagons hexes = for_ hexes $ \(poly, _val) -> plot poly

noise :: Options -> Vec2 -> Double
noise options = runST $ do
    gen <- MWC.initialize (V.fromList [])
    let Options{_canvas=Canvas{_canvasWidth=imageWidth, _canvasHeight=imageHeight}} = options
        params = def
            { _simplexFrequency = 2^5/(2*min imageWidth imageHeight)
            -- , _simplexLacunarity = 2
            , _simplexOctaves = 2
            -- , _simplexPersistence = 0.5
            }
    simplex2 params gen

-- | For each threshold (in order), return a map of values beneath that threshold.
partitionByThresholds :: [Double] -> Map Hex (Polygon, Double) -> [Map Hex (Polygon, Double)]
partitionByThresholds thresholds = go (sort thresholds)
  where
    go [] hexes = [hexes]
    go (t:ts) hexes = case M.partition (\(_polygon, val) -> val < t) hexes of
        (yes, no) -> yes : go ts no

hexPolygons :: Options -> Map Hex Int -> Map Hex (Polygon, Double)
hexPolygons options hexes = flip M.mapWithKey hexes $ \hex hexValue ->
    let maxValue = maximum hexes
        scaleFactor = lerp (0, log (fromIntegral maxValue)) (0.4,0.9) (log (fromIntegral hexValue))
        Options{_cellSize=cellSize} = options
        polyRaw = Hex.hexagonPoly cellSize hex
        poly = G.transform (G.scaleAround (polygonCentroid polyRaw) scaleFactor) polyRaw
        hexCenter = Hex.toVec2 cellSize hex
        colorValue = noise options hexCenter
    in (poly, colorValue)

gaussianHexagons :: Options -> Map Hex Int
gaussianHexagons options = runST $ do
    gen <- MWC.initialize (V.fromList [2])
    let Options{_canvas=canvas, _cellSize=cellSize, _stddev=stddev, _samples=samples} = options
        Canvas{_canvasWidth=imageWidth, _canvasHeight=imageHeight, _canvasMargin=margin} = canvas
    hexs <- replicateM samples $ do
        x <- MWC.normal (imageWidth/2) stddev gen
        y <- MWC.normal (imageHeight/2) stddev gen
        pure (Hex.fromVec2 cellSize (Vec2 x y))
    let inbounds hex = Hex.toVec2 cellSize hex `insideBoundingBox` [Vec2 margin margin, Vec2 (imageWidth-margin) (imageHeight-margin)]
    pure $ foldl' (\acc hex -> M.insertWith (+) hex (1::Int) acc) mempty (filter inbounds hexs)

data Options = Options
    { _cellSize :: Double
    , _stddev :: Double
    , _samples :: Int
    , _canvas :: Canvas
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = Options
        <$> option auto (mconcat
            [ long "cell-size"
            , metavar "[mm]"
            , value 10
            , showDefault
            , help "Cell size (hexagon radius)"
            ])
        <*> option auto (mconcat
            [ long "stddev"
            , metavar "[mm]"
            , value 50
            , showDefault
            , help "Standard deviation for sampling Gaussian points"
            ])
        <*> option auto (mconcat
            [ long "samples"
            , metavar "n"
            , value (2^16)
            , showDefault
            , help "Number of samples for the histogram"
            ])
        <*> canvasP

    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Gaussian Hexagons"
     <> header "Hexagonal histogram of Gaussian points, with two-color Simplex noise" )
