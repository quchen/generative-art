module Main (main) where



import           Control.Monad
import           Data.Coerce
import           Data.List
import qualified Data.Text.Lazy.IO        as T
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (x, y)
import           Options.Applicative
import           System.Random.MWC        (initialize)


import qualified Data.Set                        as S
import           Data.Vector                     (Vector)
import           Draw
import           Draw.Plotting
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

    render "out/vector_fields.svg" (round (_width options)) (round (_height options)) $ do

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

        plottingSettings = def { _feedrate = Just 6000, _zTravelHeight = 5, _zDrawingHeight = -2 }

    T.writeFile (_outputFileG options) $ runPlot plottingSettings drawing
    pure ()

mkGeometry :: Options -> IO [Polyline Vector]
mkGeometry options = do
    let width_mm = _width options - 2 * _margin options
        height_mm = _height options - 2 * _margin options
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
        sketch (bezierSmoothen simplified)
        stroke

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

splitIntoInsideParts :: Sequential list => Options -> Polyline list -> [[Vec2]]
splitIntoInsideParts options (Polyline xs) = filter (\(x:_) -> x `insideBoundingBox` drawBB) . groupOn (\p -> insideBoundingBox p drawBB) . toList $ xs
  where
    drawBB = boundingBox (Vec2 margin margin, Vec2 (width_mm - margin) (height_mm - margin))
    margin = _margin options
    width_mm = _width options
    height_mm = _height options


-- 2D vector potential, which in 2D is umm well a scalar potential.
vectorPotential :: Options -> Vec2 -> Double
vectorPotential options p = noiseScale *. perlin2 params p
  where
    params = PerlinParameters
        { _perlinFrequency   = 3 / (noiseScale * min (_width options) (_height options))
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
    width_mm = _width options - 2 * _margin options
    height_mm = _height options - 2 * _margin options

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
    , _width :: Double
    , _height :: Double
    , _margin :: Double
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = (\o (x,y) margin -> Options o x y margin)
        <$> strOption (mconcat
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
