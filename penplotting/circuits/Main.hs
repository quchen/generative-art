{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Default.Class
import           Data.Foldable
import           Data.List
import           Data.Set            (Set)
import qualified Data.Set            as S
import qualified Data.Text.Lazy.IO   as TL
import           Data.Traversable
import           Formatting
import           Options.Applicative
import           System.FilePath
import qualified System.Random.MWC   as MWC

import Draw.GCode
import Geometry                       as G
import Geometry.Coordinates.Hexagonal as Hex

import Circuits.GrowingProcess
import Circuits.ReconstructWires



main :: IO ()
main = do
    options <- commandLineOptions

    let lambdaScale = _lambdaScale options
        numColors = _numColors options

    let lambdaGeometry = hexLambda lambdaScale
        hexCircuits = reconstructWires (circuitProcess lambdaGeometry)
        vecCircuits = fitToPaper options (hex2wire hexCircuits)
        settings = PlottingSettings (Just (boundingBox vecCircuits)) (Just 1000)
        circuitsList = toList vecCircuits

    gen <- MWC.create
    colorIndexedCircuits <- for circuitsList $ \circuit -> do
        i <- MWC.uniformRM (1,numColors) gen
        pure (i::Int, circuit)

    let partitionByIndex
            = (map.map) snd
            . groupBy (\(i,_) (j,_) -> i == j)
            . sortBy (\(i, Wire xs) (j, Wire ys) -> compare i j <> compare (length xs) (length ys))

    for_ (zip [1..] (partitionByIndex colorIndexedCircuits)) $ \(i, wires) -> do
        let filename = formatToString (string%"_scale-"%int%"_color-"%int%"-"%int%".g") (dropExtension (_outputFileG options)) lambdaScale (i::Int) numColors
            gCodeText = renderGCode (addHeaderFooter settings (toGCode wires))
        TL.writeFile filename gCodeText

hex2wire :: Set [Hex] -> Set Wire
hex2wire = G.transform mirrorYCoords (S.map (Wire . map (toVec2 1)))

fitToPaper :: (Transform geo, HasBoundingBox geo) => Options -> geo -> geo
fitToPaper opts geo = G.transform (G.transformBoundingBox geo (Vec2 margin margin, Vec2 w h -. Vec2 margin margin) def) geo
  where
    Options {_width=w, _height=h, _margin=margin} = opts

newtype Wire = Wire [Vec2]
    deriving (Eq, Ord, Show)

instance HasBoundingBox Wire where
    boundingBox (Wire xs) = boundingBox xs

instance Transform Wire where
    transform t (Wire xs) = Wire (transform t xs)

wireToGCode :: Wire -> [GCode]
wireToGCode (Wire ws) = case ws of
    [] -> error "Bad circuit algorithm! :-C"
    [_] -> error "Bad circuit algorithm! :-C"
    xs@(Vec2 startX startY:_) -> [G00_LinearRapidMove (Just startX) (Just startY) Nothing, draw (GBlock (go xs))]
  where
    go :: [Vec2] -> [GCode]
    go [start,target] =
        let cellSize = norm (start -. target)/2
            circleRadius = cellSize/2
            Line _ intersection@(Vec2 edgeX edgeY) = resizeLine (\d -> d - circleRadius) (Line start target)
            Vec2 centerDX centerDY = target -. intersection
        in [ G01_LinearFeedrateMove (Just edgeX) (Just edgeY) Nothing
           , G91_RelativeMovement
           , G02_ArcClockwise centerDX centerDY 0 0
           , G90_AbsoluteMovement
           ]
    go (_:rest@(Vec2 x y:_)) = G01_LinearFeedrateMove (Just x) (Just y) Nothing : go rest
    go _ = error "Can’t happen because go is only called with lists of at least two elements"

instance ToGCode Wire where
    toGCode = GBlock . wireToGCode

-- | A lambda in hexagonal coordinates.
hexLambda
    :: Int -- ^ Scale parameter. c*10 will be the total height.
    -> ProcessGeometry
hexLambda c | c <= 0 = ProcessGeometry S.empty S.empty
hexLambda c = ProcessGeometry
    { _inside = pointsOnInside
    , _edge =  pointsOnEdge
    }
  where
    polygon = Hex.Polygon corners
    corners = walkInSteps
        [ id
        , move R  (c*2)
        , move DR (c*10)
        , move L  (c*2)
        , move UL (c*3)
        , move DL (c*3)
        , move L  (c*2)
        , move UR (c*5)
        ]
        (move UL (c*5) (move L c hexZero))
    walkInSteps [] _pos = []
    walkInSteps (f:fs) pos =
        let newPoint = f pos
        in newPoint : walkInSteps fs newPoint

    floodFillStart = hexZero
    floodFilled = floodFill floodFillStart (edgePoints polygon)
    pointsOnInside = floodFilled `S.difference` pointsOnEdge
    pointsOnEdge = edgePoints polygon

data Options = Options
    { _outputFileG :: FilePath

    , _numColors :: Int
    , _lambdaScale :: Int

    , _width :: Double
    , _height :: Double
    , _margin :: Double
    } deriving (Eq, Ord, Show)

commandLineOptions :: IO Options
commandLineOptions = execParser parserOpts
  where
    progOpts = (\o colors lambdaScale (x,y) margin -> Options o colors lambdaScale x y margin)
        <$> strOption (mconcat
            [ long "output"
            , short 'o'
            , metavar "<file>"
            , help "Output GCode file"
            ])
        <*> option auto (mconcat
            [ long "colors"
            , metavar "n"
            , value 1
            , showDefault
            , help "Number of colors"
            ])
        <*> option auto (mconcat
            [ long "scale"
            , metavar "n"
            , value 10
            , showDefault
            , help "Fineness of the circuit pattern. Higher is finer."
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
