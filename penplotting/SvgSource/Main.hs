{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TL
import qualified Data.Vector         as V
import           Options.Applicative
import           System.Exit

import Draw
import Draw.Plotting
import Geometry           as G
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
                    { _previewBoundingBox = Just (boundingBox paths)
                    , _feedrate = Just 1000
                    }
                gcodeRaw = runPlot plottingSettings drawing
            TL.writeFile (_outputFileG options) gcodeRaw

scaleToFit :: HasBoundingBox world => Options -> world -> Transformation
scaleToFit options world = G.transformBoundingBox world (zero +. margin2, Vec2 width height -. margin2) def
  where
    Options{_margin=margin, _height=height, _width=width} = options
    margin2 = Vec2 margin margin

polyLineLength :: Sequential vector => vector Vec2 -> Double
polyLineLength xs =
    let xsVec = toVector xs
    in V.foldl' (+) 0 (V.zipWith (\start end -> lineLength (Line start end)) xsVec (V.tail xsVec))

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

    , _width :: Double
    , _height :: Double
    , _margin :: Double
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = (\i o (x,y) margin -> Options i o x y margin)
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
        <*> asum
            [ option sizeReader $ mconcat
                [ long "size"
                , short 's'
                , metavar "[mm]"
                , help "Output size, format: <width>x<height>"
                ]
            , flag' (paper_a4_long, paper_a4_short) $ mconcat
                [ long "a4-landscape"
                , help "DIN A4, landscape orientation (271 mm × 210 mm)"
                ]
            , flag' (paper_a4_short, paper_a4_long) $ mconcat
                [ long "a4-portrait"
                , help "DIN A4, portrait orientation (210 mm × 271 mm)"
                ]
            , flag' (paper_a3_long, paper_a3_short) $ mconcat
                [ long "a3-landscape"
                , help "DIN A3, landscape orientation (420 mm × 271 mm)"
                ]
            , flag' (paper_a3_short, paper_a3_long) $ mconcat
                [ long "a3-portrait"
                , help "DIN A3, portrait orientation (271 mm × 420 mm)"
                ]
            , flag' (paper_a2_long, paper_a2_short) $ mconcat
                [ long "a2-landscape"
                , help "DIN A2, landscape orientation (594 mm × 420 mm)"
                ]
            , flag' (paper_a2_short, paper_a2_long) $ mconcat
                [ long "a2-portrait"
                , help "DIN A2, portrait orientation (420 mm × 594 mm)"
                ]
            ]
        <*> option auto (mconcat
            [ long "margin"
            , metavar "[mm]"
            , value 10
            , showDefaultWith (\x -> show x <> " mm")
            , help "Ensure this much blank space to the edge"
            ])

    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Convert SVG to GCode"
     <> header "Not that much of SVG is supported, bear with me…" )

    sizeReader :: ReadM (Double, Double)
    sizeReader = do
            w <- auto
            _ <- eitherReader $ \case
                "x" -> Right ()
                "×" -> Right ()
                _ -> Left "expected width/height separator: x"
            h <- auto
            pure (w,h)

paper_a4_long, paper_a4_short, paper_a3_short, paper_a3_long, paper_a2_short, paper_a2_long :: Double
paper_a4_long = 297
paper_a4_short = 210
paper_a3_short = paper_a4_long
paper_a3_long = 420
paper_a2_short = paper_a3_long
paper_a2_long = 594
