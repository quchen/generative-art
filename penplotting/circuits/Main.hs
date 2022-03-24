module Main (main) where



import           Data.Default.Class
import           Data.Foldable
import           Data.List
import           Data.Set           (Set)
import qualified Data.Set           as S
import qualified Data.Text.Lazy.IO  as TL
import           Data.Traversable
import qualified System.Random.MWC  as MWC

import Draw.GCode
import Geometry                       as G
import Geometry.Coordinates.Hexagonal as Hex

import Circuits.GrowingProcess
import Circuits.ReconstructWires



main :: IO ()
main = do
    let lambdaScale = 10
        numColors = 3

        lambdaGeometry = hexLambda lambdaScale

        hexCircuits = reconstructWires (circuitProcess lambdaGeometry)

        vecCircuits = fitToPaper (hex2wire hexCircuits)

        settings = PlottingSettings (Just (boundingBox vecCircuits)) (Just 1000)

        circuitsList = toList vecCircuits

    gen <- MWC.create
    colorIndexedCircuits <- for circuitsList $ \circuit -> do
        i <- MWC.uniformRM (1,numColors) gen
        pure (i::Int, circuit)

    let partitionByIndex :: [(Int, a)] -> [[a]]
        partitionByIndex = (map.map) snd . groupBy (\(i,_) (j,_) -> i == j) . sortBy (\(i,_) (j,_) -> compare i j)

    for_ (zip [1..] (partitionByIndex colorIndexedCircuits)) $ \(i, wires) -> do
        TL.writeFile
            ("/home/main/temp/circuit-lambda-scale-"++show lambdaScale++"-color-"++show i++".g")
            (renderGCode (addHeaderFooter settings (toGCode wires)))

hex2wire :: Set [Hex] -> Set Wire
hex2wire = S.map (Wire . map (toVec2 1))

fitToPaper :: (Transform geo, HasBoundingBox geo) => geo -> geo
fitToPaper geo = G.transform (G.transformBoundingBox geo (Vec2 margin margin, Vec2 210 291 -. Vec2 margin margin) def) geo
  where
    margin = 10

newtype Wire = Wire [Vec2]
    deriving (Eq, Ord, Show)

instance HasBoundingBox Wire where
    boundingBox (Wire xs) = boundingBox xs

instance Transform Wire where
    transform t (Wire xs) = Wire (transform t xs)

wireToGCode :: Wire -> [GCode]
wireToGCode (Wire ws) = case ws of
    [] -> error "Bad circuit algorithm! :-("
    [_] -> error "Bad circuit algorithm! :-("
    xs@(Vec2 startX startY:_) -> [G00_LinearRapidMove (Just startX) (Just startY) Nothing, draw (GBlock (go xs))]
  where
    go :: [Vec2] -> [GCode]
    go [start,target] =
        let cellSize = norm (start -. target)/2
            circleRadius = cellSize/2
            Line _ intersection@(Vec2 edgeX edgeY) = resizeLine (\d -> d - circleRadius) (Line start target)
            Vec2 centerDX centerDY = target -. intersection
        in [ G01_LinearFeedrateMove (Just edgeX) (Just edgeY) Nothing
           , G02_ArcClockwise centerDX centerDY edgeX edgeY
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
