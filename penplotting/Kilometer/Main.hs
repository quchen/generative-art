module Main (main) where



import           Data.Default.Class
import           Data.Foldable
import qualified Data.Text.Lazy.IO      as TL
import           Draw.Plotting
import           Draw.Plotting.CmdArgs
import           Geometry               as G
import           Numerics.Interpolation
import           Options.Applicative





data Options = Options
    { _lineLength_m :: Double

    , _canvas :: Canvas
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = Options
        <$> option auto (mconcat
            [ long "length"
            , short 'l'
            , metavar "<m>"
            , help "Total line length"
            ])
        <*> canvasP

    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Paint a certain length of line on a piece of paper." )

main :: IO ()
main = do
    options <- commandLineOptions
    let Options {_lineLength_m = totalLength_m, _canvas = Canvas {_canvasHeight=height_mm, _canvasWidth=width_mm, _canvasMargin=margin_mm}} = options
    let geometry = lotsOfLines totalLength_m (boundingBox (Vec2 margin_mm margin_mm, Vec2 (width_mm - margin_mm) (height_mm-margin_mm)))
    let plotResult = runPlot def $ do
            let flipEveryOtherLine = zipWith ($) (cycle [id, lineReverse])
            for_ (flipEveryOtherLine geometry) plot
    TL.putStrLn (renderGCode (_plotGCode plotResult))

lotsOfLines :: Double -> BoundingBox -> [Line]
lotsOfLines totalLength_m bb = do
    let (singleLineLength, _) = boundingBoxSize bb
        BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax) = bb
        numLines = floor (totalLength_m * 1000 / singleLineLength)
    n <- [1..numLines]
    let y = lerp (1, fromIntegral numLines) (yMin, yMax) (fromIntegral n)
    pure (Line (Vec2 xMin y) (Vec2 xMax y))
