module Main (main) where



import           Control.Monad
import           Data.Coerce
import           Data.List
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Options.Applicative
import           System.Random.MWC        (initialize)


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



-- Higher values yield lower-frequency noise
noiseScale :: Double
noiseScale = 0.5

noiseSeed :: Int
noiseSeed = 519496

main :: IO ()
main = do
    options <- commandLineOptions
    geometry <- mkGeometry options

    let (width, height, _) = widthHeightMargin options
    render "out/vector_fields.svg" (round width) (round height) $ do

        cairoScope $ do
            setColor black
            C.paint

        cairoScope $ do
            setColor white
            setLineWidth 0.3
            for_ geometry drawFieldLine

    let drawing = sequence
            [ plot (Polyline part)
            | trajectory <- geometry
            , part <- simplifyTrajectoryRadial 2 <$> splitIntoInsideParts options trajectory ]

        plottingSettings = def { _feedrate = 6000, _zTravelHeight = 5, _zDrawingHeight = -2 }

    writeGCodeFile (_outputFileG options) (runPlot plottingSettings drawing)

mkGeometry :: Options -> IO [Polyline Vector]
mkGeometry options = do
    let (width_opt, height_opt, margin_opt) = widthHeightMargin options
        width_mm = width_opt - 2 * margin_opt
        height_mm = height_opt - 2 * margin_opt
    gen <- initialize (V.fromList [fromIntegral noiseSeed])
    startPoints <- poissonDisc gen PoissonDiscParams
        { _poissonShape = boundingBox [ Vec2 (-50) 0, Vec2 (width_mm + 50) (height_mm / 10) ]
        , _poissonK = 3
        , _poissonRadius = 5
        }
    let mkTrajectory start =
              Polyline
            . map (\(_t, pos) -> pos)
            . takeWhile
                (\(t, pos) -> t <= 200 && pos `insideBoundingBox` (Vec2 (-50) (-50), Vec2 (width_mm+50) (height_mm+50)))
            $ fieldLine (velocityField options) (G.transform (G.scale' 1 10) start)
    pure ((coerce . minimizePenHovering . S.fromList . concatMap (splitIntoInsideParts options . mkTrajectory)) startPoints)

drawFieldLine :: Polyline Vector -> Render ()
drawFieldLine (Polyline polyLine) = cairoScope $ do
    let simplified = simplifyTrajectoryRadial 2 polyLine
    unless (null (drop 2 simplified)) $ do
        sketch (PolyBezier (bezierSmoothen simplified))
        stroke

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

splitIntoInsideParts :: Sequential list => Options -> Polyline list -> [[Vec2]]
splitIntoInsideParts options (Polyline xs) = filter (\(x:_) -> x `insideBoundingBox` drawBB) . groupOn (\p -> insideBoundingBox p drawBB) . toList $ xs
  where
    drawBB = boundingBox (Vec2 margin margin, Vec2 (width - margin) (height - margin))
    (width, height, margin) = widthHeightMargin options


-- 2D vector potential, which in 2D is umm well a scalar potential.
vectorPotential :: Options -> Vec2 -> Double
vectorPotential options p = noiseScale *. perlin2 params p
  where
    (width, height, _) = widthHeightMargin options
    params = PerlinParameters
        { _perlinFrequency   = 3 / (noiseScale * min width height)
        , _perlinLacunarity  = 2
        , _perlinOctaves     = 1
        , _perlinPersistence = 0.5
        , _perlinSeed        = noiseSeed
        }

rotationField :: Options -> Vec2 -> Vec2
rotationField options = curlZ (vectorPotential options)

velocityField :: Options -> Vec2 -> Vec2
velocityField options p@(Vec2 x y) = Vec2 1 0 +. perturbationStrength *. rotationField options p
  where
    perturbationStrength =
        0.8
        * logisticRamp (0.6*width_mm) (width_mm/6) x
        * gaussianFalloff (0.5*height_mm) (0.4*height_mm) y

    (width, height, margin) = widthHeightMargin options
    width_mm = width - 2 * margin
    height_mm = height - 2 * margin

widthHeightMargin :: Options -> (Double, Double, Double)
widthHeightMargin Options{_canvas=Canvas{_canvasWidth=width, _canvasHeight=height, _canvasMargin=margin}} = (width, height, margin)

fieldLine
    :: (Vec2 -> Vec2)
    -> Vec2
    -> [(Double, Vec2)]
fieldLine f p0 = rungeKuttaAdaptiveStep (ODE.fieldLine f) p0 t0 dt0 tolNorm tol
  where
    t0 = 0
    dt0 = 1
    -- Decrease exponent for more accurate results
    tol = 1e-4
    tolNorm = norm

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
