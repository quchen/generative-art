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



scaleToA4Portrait :: (Transform geo, HasBoundingBox geo) => geo -> geo
scaleToA4Portrait geo = G.transform (G.transformBoundingBox geo (Vec2 0 0 +. margin, Vec2 a4width_mm a4height_mm -. margin) def) geo
  where
    margin = Vec2 10 10
    a4height_mm = 210
    a4width_mm = 297

main :: IO ()
main = do
    Options {_inputFileSvg=inputFileSvg, _outputFileG=outputFileG} <- commandLineOptions
    inputSvg <- T.readFile inputFileSvg
    let inputLines = T.lines inputSvg
    case fmap concat (traverse parse inputLines) of
        Left err -> T.putStrLn ("Parse error: " <> err)
        Right paths -> do
            let scaled = sortBy (\x y -> compare (polyLineLength x) (polyLineLength y)) (scaleToA4Portrait (G.transform mirrorXCoords (extractPolylines paths)))
                gcode = GBlock
                    [ GComment ("Total line length: " <> TL.pack (show (sum (map polyLineLength scaled))))
                    , GComment ("Number of polylines: " <> TL.pack (show (length scaled)))
                    , convertToGcode scaled
                    ]
                gcodeRaw = renderGCode gcode
            TL.writeFile outputFileG gcodeRaw

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
