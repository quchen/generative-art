{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import qualified Data.Text                       as T
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy.IO               as TL
import           Options.Applicative
import Data.List

import           Draw
import           Draw.GCode
import           Geometry                        as G
import           Geometry.SvgPathParser



scaleToFit :: (Transform geo, HasBoundingBox geo) => Options -> geo -> geo
scaleToFit options geo = G.transform (G.transformBoundingBox geo (Vec2 0 0 +. margin2, Vec2 width height -. margin2) def) geo
  where
    Options{_margin=margin, _height=height, _width=width} = options
    margin2 = Vec2 margin margin

main :: IO ()
main = do
    options <- commandLineOptions
    inputSvg <- T.readFile (_inputFileSvg options)
    let inputLines = T.lines inputSvg
    case fmap concat (traverse parse inputLines) of
        Left err -> T.putStrLn ("Parse error: " <> err)
        Right paths -> do
            let scaled =
                      map (\(_len, polyline) -> polyline)
                    . sortBy (\(len1, _) (len2, _) -> compare len1 len2)
                    . filter (\(len, _) -> len >= 1)
                    . map (\polyline -> (polyLineLength polyline, polyline))
                    . scaleToFit options
                    . G.transform mirrorYCoords
                    . extractPolylines
                    $ paths
                gcode = GBlock
                    [ GComment ("Total line length: " <> TL.pack (show (sum (map polyLineLength scaled))))
                    , GComment ("Number of polylines: " <> TL.pack (show (length scaled)))
                    , convertToGcode scaled
                    ]
                gcodeRaw = renderGCode gcode
            TL.writeFile (_outputFileG options) gcodeRaw

polyLineLength :: [Vec2] -> Double
polyLineLength xs = sum (zipWith (\start end -> lineLength (Line start end)) xs (tail xs))

convertToGcode :: (ToGCode a, HasBoundingBox a) => [a] -> GCode
convertToGcode polylines =
    let collectionOfGcodeLines = map toGCode polylines

        plottingSettings = PlottingSettings
            { _previewBoundingBox = Just (boundingBox polylines)
            , _feedrate = Just 1000
            }
        gcode = addHeaderFooter plottingSettings (GBlock collectionOfGcodeLines)
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
    progOpts = Options
        <$> strOption   (mconcat
            [ long "input"
            , short 'f'
            , metavar "<file>"
            , help "Input SVG file"
            ])
        <*> strOption   (mconcat
            [ long "output"
            , short 'o'
            , metavar "<file>"
            , help "Output GCode file"
            ])
        <*> option auto (mconcat
            [ long "width"
            , short 'w'
            , metavar "[mm]"
            , help "Output width, e.g. 271 for DIN A4 (landscape)"
            ])
        <*> option auto (mconcat
            [ long "height"
            , short 'h'
            , metavar "[mm]"
            , help "Output height, e.g. 210 for DIN A4 (landscape)"
            ])
        <*> option auto (mconcat
            [ long "margin"
            , metavar "[mm]"
            , value 0
            , showDefault
            , help "Margin; ensure this much blank space to the edge of the output"
            ])

    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Convert SVG to GCode"
     <> header "Not that much of SVG is supported, bear with me…" )
