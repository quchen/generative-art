{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Text.Lazy      as TL
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
            let transformAll :: (HasBoundingBox geo, Transform geo) => geo -> geo
                transformAll = G.transform (scaleToFit options (boundingBox svgElements))
                paths =
                      filter (\polyline -> polyLineLength polyline >= 1)
                    . minimizePenHovering
                    . S.fromList
                    . transformAll
                    . concatMap pathToPolyline
                    $ svgElements

                numElements = length paths
                totalLength = sum (fmap polyLineLength paths)

                drawing = block $ do
                    comment ("Total line length: " <> TL.pack (show totalLength))
                    comment ("Number of elements to draw: " <> TL.pack (show numElements))
                    plot (Polyline <$> paths)
                plottingSettings = def
                    { _feedrate = 1000
                    , _zLoweringFeedrate = Just 1000
                    , _zTravelHeight = 2
                    , _zDrawingHeight = -2
                    , _finishMove = Just FinishWithG28
                    }
            writeGCodeFile (_outputFileG options) (runPlot plottingSettings drawing)

scaleToFit :: HasBoundingBox world => Options -> world -> Transformation
scaleToFit options world = G.transformBoundingBox world (zero +. margin2, Vec2 width height -. margin2) def
  where
    Options{_canvas=Canvas{_canvasMargin=margin, _canvasHeight=height, _canvasWidth=width}} = options
    margin2 = Vec2 margin margin

polyLineLength :: Sequential list => list Vec2 -> Double
polyLineLength xs =
    let xsList = toList xs
    in foldl' (+) 0 (zipWith (\start end -> lineLength (Line start end)) xsList (tail (cycle xsList)))

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

    , _canvas :: Canvas
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = Options
        <$> strOption (mconcat
            [ long "input"
            , short 'f'
            , metavar "<file>"
            , help "Input SVG file"
            ])
        <*> strOption (mconcat
            [ long "output"
            , short 'o'
            , metavar "<file>"
            , help "Output GCode file"
            ])
        <*> canvasP

    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Convert SVG to GCode"
     <> header "Not that much of SVG is supported, bear with me…" )
