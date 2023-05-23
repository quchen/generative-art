module Main (main) where



import           Control.Monad
import           Data.Coerce
import           Data.List
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Options.Applicative
import  qualified          System.Random.MWC    as MWC


import qualified Data.Set                        as S
import           Data.Vector                     (Vector)
import           Draw
import           Draw.Plotting
import           Draw.Plotting.CmdArgs
import           Geometry                        as G
import           Geometry.Algorithms.PerlinNoise
import           Geometry.Algorithms.Sampling
import qualified Geometry.Processes.FlowField    as ODE
import           Numerics.DifferentialEquation
import           Numerics.Functions
import           Numerics.VectorAnalysis
import Geometry.Algorithms.Delaunay



main :: IO ()
main = do
    let width = 228
        height = 150

        outfileG = "out/postcard-delaunay.g"
        outfilePreview = "out/postcard.svg"

        margin = 10
        paperBB = boundingBox [zero, Vec2 width height]
        drawBB = shrinkBoundingBox margin paperBB

    points <- do
        gen <- MWC.initialize (V.fromList [])
        ps <- poissonDisc gen PoissonDiscParams
            { _poissonShape = drawBB
            , _poissonRadius = 5
            , _poissonK = 10
            }
        let ps' = filter (\p -> norm (boundingBoxCenter drawBB -. p) < 59) ps
        pure (lloydRelaxation drawBB 0.1 ps')

    let delaunay = delaunayTriangulation points

        optimize :: Vector Line -> [Polyline Vector]
        optimize lines1 =
            let foo = S.fromList $ toList $ V.map (\(Line start end) -> V.fromList [start, end]) lines1
            in fmap Polyline (minimizePenHovering foo)

        plottingSettings = def { _zTravelHeight = 5 }
        plotResult = runPlot plottingSettings $ do
            -- let polylines = optimize ()
            let hullPoints = S.fromList . toList . delaunayHull $ delaunay
                paintUs = do
                    line@(Line start end) <- delaunayEdges delaunay
                    guard (all (\p -> S.notMember p hullPoints) [start, end] || True)
                    pure line
            for_ (optimize paintUs) plot

    writeGCodeFile outfileG plotResult
    renderPreview outfilePreview 3 plotResult
    pure ()


widthHeightMargin :: Options -> (Double, Double, Double)
widthHeightMargin Options{_canvas=Canvas{_canvasWidth=width, _canvasHeight=height, _canvasMargin=margin}} = (width, height, margin)

data Options = Options
    { _outputFileG :: FilePath
    , _canvas :: Canvas
    } deriving (Eq, Ord, Show)


commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = Options
        <$> strOption (mconcat
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
