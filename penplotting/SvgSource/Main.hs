{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy.IO               as TL
import           Options.Applicative

import           Draw
import           Draw.GCode
import           Geometry                        as G
import           Geometry.SvgPathParser



scaleToA4Portrait :: (Transform geo, HasBoundingBox geo) => geo -> geo
scaleToA4Portrait geo = G.transform (G.transformBoundingBox geo (Vec2 0 0 +. margin, Vec2 a4width_mm a4height_mm -. margin) def) geo
  where
    margin = Vec2 10 10
    a4width_mm = 210
    a4height_mm = 297

main :: IO ()
main = do
    Options {_inputFileSvg=inputFileSvg, _outputFileG=outputFileG} <- commandLineOptions
    inputSvg <- T.readFile inputFileSvg
    let inputLines = T.lines inputSvg
    case fmap concat (traverse parse inputLines) of
        Left err -> T.putStrLn ("Parse error: " <> err)
        Right paths -> do
            let scaled = scaleToA4Portrait (G.transform mirrorXCoords (extractPolylines paths))
                gcode = convertToGcode scaled
                gcodeRaw = renderGCode gcode
            TL.writeFile outputFileG gcodeRaw

convertToGcode :: (ToGCode a, HasBoundingBox a) => [a] -> GCode
convertToGcode polylines =
    let collectionOfGcodeLines = map toGCode polylines

        plottingSettings = PlottingSettings
            { _previewBoundingBox = Just (boundingBox polylines)
            , _feedrate = Just 1000
            }
        gcode = addHeaderFooter plottingSettings (GBlock collectionOfGcodeLines)
    in gcode

extractPolylines :: [[Either Line noBeziersPlease]] -> [[Vec2]]
extractPolylines paths =
    let svgLines = map (\path -> map (\(Left p) -> p) path) paths
        lineListToPolyline :: [Line] -> [Vec2]
        lineListToPolyline [] = []
        lineListToPolyline (Line x y : xs) = x : y : [end | Line _ end <- xs]
        polylines = map lineListToPolyline svgLines
    in polylines

data Options = Options
    { _inputFileSvg :: FilePath
    , _outputFileG :: FilePath
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = Options
        <$> strOption (
               long "input"
            <> short 'f'
            <> metavar "FILE"
            <> help "Input SVG file")
        <*> strOption (
               long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output GCode file")
    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Convert SVG to GCode"
     <> header "Not that much of SVG is supported, bear with me…" )
