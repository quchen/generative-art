{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Default.Class
import           Data.Foldable
import           Data.List
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Traversable
import           Formatting
import           Options.Applicative
import           System.FilePath
import qualified System.Random.MWC   as MWC

import Draw.Plotting
import Draw.Plotting.CmdArgs
import Draw.Plotting.GCode
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
        settings = def { _feedrate = Just 1000 }
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
        writeGCodeFile filename (runPlot settings (plot wires))

hex2wire :: Set [Hex] -> Set Wire
hex2wire = G.transform mirrorYCoords (S.map (Wire . map (toVec2 1)))

fitToPaper :: (Transform geo, HasBoundingBox geo) => Options -> geo -> geo
fitToPaper opts geo = G.transform (G.transformBoundingBox geo (Vec2 margin margin, Vec2 w h -. Vec2 margin margin) def) geo
  where
    Options {_canvas=Canvas{_canvasWidth=w, _canvasHeight=h, _canvasMargin=margin}} = opts

newtype Wire = Wire [Vec2]
    deriving (Eq, Ord, Show)

instance HasBoundingBox Wire where
    boundingBox (Wire xs) = boundingBox xs

instance Transform Wire where
    transform t (Wire xs) = Wire (transform t xs)

drawWire :: Wire -> Plot ()
drawWire (Wire ws) = case ws of
    [] -> error "Bad circuit algorithm! :-C"
    [_] -> error "Bad circuit algorithm! :-C"
    xs@(start:_) -> do
        repositionTo start
        block (go xs)
  where
    go :: [Vec2] -> Plot ()
    go [start,target] = do
        let cellSize = norm (start -. target)/2
            circleRadius = cellSize/2
            Line _ intersection = resizeLine (\d -> d - circleRadius) (Line start target)
            Vec2 centerDX centerDY = target -. intersection
        plot (Line start intersection)
        gCode
            [ G91_RelativeMovement
            , G02_ArcClockwise Nothing centerDX centerDY 0 0
            , G90_AbsoluteMovement
            ]
    go (_:rest@(target:_)) = lineTo target >> go rest
    go _ = error "Can’t happen because go is only called with lists of at least two elements"

instance Plotting Wire where
    plot = block . drawWire

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
        <*> canvasP

    parserOpts = info (progOpts <**> helper)
      ( fullDesc
     <> progDesc "Convert SVG to GCode"
     <> header "Not that much of SVG is supported, bear with me…" )
