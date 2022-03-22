{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.List
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TL
import           Options.Applicative
import           System.Exit

import Draw
import Draw.GCode
import Geometry           as G
import Geometry.Shapes
import Geometry.SvgParser



scaleToFit :: HasBoundingBox world => Options -> world -> Transformation
scaleToFit options world = G.transformBoundingBox world (zero +. margin2, Vec2 width height -. margin2) def
  where
    Options{_margin=margin, _height=height, _width=width} = options
    margin2 = Vec2 margin margin

main :: IO ()
main = do
    options <- commandLineOptions
    print options
    inputSvg <- T.readFile (_inputFileSvg options)
    let inputLines = T.lines inputSvg
    case traverse parse inputLines of
        Left err -> T.putStrLn ("Parse error: " <> err) >> exitWith (ExitFailure 1)
        Right svgElementsYflipped -> do
            let svgElements = G.transform mirrorYCoords svgElementsYflipped
            let transformAll :: (HasBoundingBox geo, Transform geo) => geo -> geo
                transformAll = G.transform (scaleToFit options (boundingBox svgElements))
                paths =
                      map (\(_len, polyline) -> polyline)
                    . sortBy (\(len1, _) (len2, _) -> compare len1 len2)
                    . filter (\(len, _) -> len >= 1)
                    . map (\polyline -> (polyLineLength polyline, polyline))
                    . transformAll
                    . extractPolylines
                    . concat
                    $ [path | SvgPath path <- svgElements]
                simpleLines = transformAll [line | SvgLine line <- svgElements]
                simpleEllipses = transformAll [ellipse | SvgEllipse ellipse <- svgElements]

                numElements = length paths + length simpleLines + length simpleEllipses
                totalLength = sum
                    [ sum (map polyLineLength paths)
                    , sum (map lineLength simpleLines)
                    , sum (map circumferenceEllipse simpleEllipses)
                    ]

                gcode = GBlock
                    [ GComment ("Total line length: " <> TL.pack (show totalLength))
                    , GComment ("Number of elements to draw: " <> TL.pack (show numElements))
                    , convertToGcode (simpleLines, simpleEllipses, paths)
                    ]
                gcodeRaw = renderGCode gcode
            TL.writeFile (_outputFileG options) gcodeRaw

polyLineLength :: [Vec2] -> Double
polyLineLength xs = sum (zipWith (\start end -> lineLength (Line start end)) xs (tail xs))

circumferenceEllipse :: Ellipse -> Double
circumferenceEllipse (Ellipse e) = polygonCircumference (transform e (regularPolygon 100)) -- lol

convertToGcode :: (ToGCode a, HasBoundingBox a) => a -> GCode
convertToGcode elements =
    let plottingSettings = PlottingSettings
            { _previewBoundingBox = Just (boundingBox elements)
            , _feedrate = Just 1000
            }
        gcode = addHeaderFooter plottingSettings (toGCode elements)
    in gcode

pathToLineSegments :: [Either Line Bezier] -> [Vec2]
pathToLineSegments [] = []
pathToLineSegments [Left (Line x y)] = [x,y]
pathToLineSegments (Left (Line x _) : xs) = x : pathToLineSegments xs
pathToLineSegments [Right bezier] = bezierSubdivideT 32 bezier
pathToLineSegments (Right bezier : xs) = init (bezierSubdivideT 32 bezier) ++ pathToLineSegments xs

extractPolylines :: [[Either Line Bezier]] -> [[Vec2]]
extractPolylines paths = map (removeDuplicates . pathToLineSegments) paths

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = map head . group

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
            , flag' (297, 210) $ mconcat
                [ long "a4-landscape"
                , help "DIN A4, landscape orientation (271 mm × 210 mm)"
                ]
            , flag' (210, 297) $ mconcat
                [ long "a4-portrait"
                , help "DIN A4, portrait orientation (210 mm × 271 mm)"
                ]
            , flag' (420, 297) $ mconcat
                [ long "a3-landscape"
                , help "DIN A3, landscape orientation (420 mm × 271 mm)"
                ]
            , flag' (297, 420) $ mconcat
                [ long "a3-portrait"
                , help "DIN A3, portrait orientation (271 mm × 420 mm)"
                ]
            , flag' (594, 420) $ mconcat
                [ long "a2-landscape"
                , help "DIN A2, landscape orientation (594 mm × 420 mm)"
                ]
            , flag' (420, 594) $ mconcat
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
