{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Maybe
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Applicative
import           System.Exit

import Draw
import Draw.Plotting
import Draw.Plotting.CmdArgs
import Geometry              as G
import Geometry.Shapes
import Geometry.SvgParser



main :: IO ()
main = do
    options <- commandLineOptions
    print options
    inputSvg <- T.readFile (_inputFileSvg options)
    let inputLines = T.lines inputSvg
    case traverse parse inputLines of
        Left err -> T.putStrLn ("Parse error: " <> err) >> exitWith (ExitFailure 1)
        Right svgElements -> do
            let -- SVG has zero on the top left. We mirror the Y axis to align the axis before doing all the fitting transformations.
                svgElementsYMirrored = G.transform mirrorYCoords svgElements
                polylines =
                      mapMaybe (lineLongEnough options)
                    . map Polyline
                    . optimizePaths options
                    . G.transform (transformToWorld options svgElementsYMirrored)
                    . concatMap pathToPolyline
                    $ svgElementsYMirrored

                plottingSettings = def
                    { _feedrate = 1000
                    , _zLoweringFeedrate = Just 1000
                    , _zTravelHeight = 2
                    , _zDrawingHeight = -1
                    , _finishMove = Just FinishWithG28
                    , _canvasBoundingBox = Just (boundingBox (_canvas options))
                    }
                plotResult = runPlot plottingSettings (plot polylines)
            writeGCodeFile (_outputFileG options) plotResult
            case _previewFile options of
                Nothing -> pure ()
                Just svgFilePath -> renderPreview svgFilePath plotResult

lineLongEnough :: Sequential f => Options -> Polyline f -> Maybe (Polyline f)
lineLongEnough options polyline = case _minimumLineLength options of
    Just minLength
        | polylineLength polyline < minLength -> Nothing
    _otherwise -> Just polyline

optimizePaths :: Options -> [[Vec2]] -> [[Vec2]]
optimizePaths options
    | _minimizePenTravel options = fmap toList . minimizePenHovering . S.fromList . toList
    | otherwise = id

transformToWorld :: HasBoundingBox drawing => Options -> drawing -> Transformation
transformToWorld options drawing = G.transformBoundingBox drawing world def
  where
    Options{_canvas=Canvas{_canvasMargin=margin, _canvasHeight=height, _canvasWidth=width}} = options
    margin2 = Vec2 margin margin
    world = (zero +. margin2, Vec2 width height -. margin2)

pathToPolyline :: SvgElement -> [[Vec2]]
pathToPolyline (SvgPath paths) = map pathToLineSegments paths
pathToPolyline (SvgLine (Line x y)) = [[x,y]]
pathToPolyline (SvgEllipse (Ellipse e)) = transform e [[x,y] | Line x y <- polygonEdges (regularPolygon 128)]

pathToLineSegments :: [Either Line Bezier] -> [Vec2]
pathToLineSegments [] = []
pathToLineSegments [Left (Line x y)] = [x,y]
pathToLineSegments (Left (Line x _) : xs) = x : pathToLineSegments xs
pathToLineSegments [Right bezier] = bezierSubdivideT 32 bezier
pathToLineSegments (Right bezier : xs) = init (bezierSubdivideT 32 bezier) ++ pathToLineSegments xs

data Options = Options
    { _inputFileSvg :: FilePath
    , _outputFileG :: FilePath
    , _previewFile :: Maybe FilePath
    , _minimizePenTravel :: Bool
    , _minimumLineLength :: Maybe Double

    , _canvas :: Canvas
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = Options
        <$> (strOption . mconcat)
            [ long "input"
            , short 'f'
            , metavar "<file>"
            , help "Input SVG file"
            ]
        <*> (strOption . mconcat)
            [ long "output"
            , short 'o'
            , metavar "<file>"
            , help "Output GCode file"
            ]
        <*> (optional . strOption . mconcat)
            [ long "preview"
            , metavar "<file>"
            , help "Output preview file (.svg or .png)"
            ]
        <*> (switch . mconcat)
            [ long "minimize-pen-travel"
            , help "Reorder lines so pen travelling is minimized. Scales quadratically, so does not work well for large number of lines."
            ]
        <*> (optional . option auto . mconcat)
            [ long "min-line-length"
            , metavar "<mm>"
            , help "Minimum line length: filter out polylines shorter than this"
            ]
        <*> canvasP

    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Convert SVG to GCode"
     <> header "Not that much of SVG is supported, bear with me…" )
