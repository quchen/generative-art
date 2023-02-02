{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where



import           Data.List
import qualified Data.MultiSet as M
import           Data.Ord
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Prelude                  hiding ((**))
import           System.Random.MWC

import Data.Grid.Hexagonal as Grid
import Draw hiding (toList)
import Draw.Grid
import Geometry                     as G
import Geometry.Coordinates.Hexagonal
import Geometry.Algorithms.WaveFunctionCollapse
import Control.Monad.ST (runST)
import Graphics.Rendering.Cairo.SVG as SVG
import qualified Graphics.Rendering.Cairo.SVG as SVG
import Control.Monad
import Data.Maybe



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

cellSize :: Num a => a
cellSize = 128

main :: IO ()
main = do
    let generations = runST $ do
            gen <- create
            wfc wfcSettings gen
    drawProtoTile <- tileSvg
    render "showcases/wfc/template.png" picWidth picHeight $ do
        cairoScope (setColor white >> paint)
        C.translate (picWidth/2) (picHeight/2)
        hexagonalCoordinateSystem cellSize (max picWidth picHeight `div` cellSize)
        for_ (toList (last generations)) $ \(hex, superposition) -> do
            drawSuperposition [ drawTile drawProtoTile tile hex | tile <- M.toList superposition ]

drawSuperposition :: [Render ()] -> Render ()
drawSuperposition [] = pure ()
drawSuperposition actions = cairoScope $ do
    let s = length actions
    for_ actions $ grouped (paintWithAlpha (1 / fromIntegral s))

drawTile :: (ProtoTile -> Render ()) -> Tile -> Hex -> Render ()
drawTile drawProtoTile tile hex = cairoScope $ do
    let Vec2 x y = toVec2 cellSize hex
    C.translate x y
    for_ (stack tile) $ \(RotatedTile pt d) -> do
        C.rotate (fromIntegral (fromEnum d) * pi/3)
        drawProtoTile pt
 


wfcSettings :: WfcSettings Hex Tile
wfcSettings = WfcSettings {..}
  where
    wfcRange = hexagonsInRange 2 hexZero
    wfcTiles = M.fromList allTiles
    wfcLocalProjection :: Grid Hex (Touched (M.MultiSet Tile)) -> Touched (M.MultiSet Tile)
    wfcLocalProjection grid
        | all (isUntouched . snd) neighbours
        = Untouched oldState
        | newState == oldState
        = Untouched oldState
        | otherwise
        = Touched newState
      where
        oldState = getTouched (extract grid)
        neighbours = [ (d, extract <$> goto d grid) | d <- valuesOf ]
        isUntouched = \case
            Nothing -> True
            Just (Untouched _) -> True
            _otherwise -> False
        newState = M.fromAscOccurList
            [ (tile, n)
            | (tile, n) <- M.toAscOccurList oldState
            , and
                [ maybe True (any isCompatible . M.toList . getTouched) neighbour
                | (d, neighbour) <- neighbours
                , let isCompatible = connects d tile
                ]
            ]



allTiles :: [Tile]
allTiles = concat
    [ [Tile []]
    , simpleTiles
    ]

simpleTiles :: [Tile]
simpleTiles =
    [ Tile [RotatedTile pt d]
    | pt <- valuesOf :: [ProtoTile]
    , d  <- valuesOf :: [Direction]
    ]

powerset :: [a] -> [[a]]
powerset [] = []
powerset [x] = [[], [x]]
powerset (x:xs) = do
    ys <- powerset xs
    [ys, x:ys]

valuesOf :: (Enum a, Bounded a) => [a]
valuesOf = enumFrom minBound


data ProtoTile
    = Corner
    | Triple
    | Straight
    | End
    deriving (Eq, Ord, Show, Bounded, Enum)

tileSvg :: IO (ProtoTile -> Render ())
tileSvg = do
    corner   <- svgNewFromFile "corner.svg"
    triple   <- svgNewFromFile "triple.svg"
    straight <- svgNewFromFile "straight.svg"
    end      <- svgNewFromFile "end.svg"
    pure $ \tile -> cairoScope $ do
        C.translate (-cellSize * sqrt 3 / 2) (-cellSize)
        void $ case tile of
            Corner   -> svgRender corner
            Triple   -> svgRender triple
            Straight -> svgRender straight
            End      -> svgRender end

-- | R = 0°, DR = 60°, etc.
data RotatedTile = RotatedTile ProtoTile Direction
    deriving (Eq, Ord, Show)

newtype Tile = Tile { stack :: [RotatedTile] }
    deriving (Eq, Ord, Show)

newtype Connector = Connector (Bool, Bool, Bool)
    deriving (Eq, Ord, Show)

instance Semigroup Connector where
    Connector (a1, b1, c1) <> Connector (a2, b2, c2) = Connector (a1 || a2, b1 || b2, c1 || c2)

instance Monoid Connector where
    mempty = Connector (False, False, False)

mirror :: Connector -> Connector
mirror (Connector (a, b, c)) = Connector (c, b, a)

class Connects tile where
    connector :: tile -> Direction -> Connector

    connects :: Direction -> tile -> tile -> Bool
    connects d t1 t2 = connector t1 d == mirror (connector t2 (opposite d))

instance Connects ProtoTile where
    connector = \case
        Corner -> \case
            L  -> cMiddle
            DR -> cMiddle
            _  -> cNone
        Triple -> \case
            UL -> cMiddle
            R  -> cMiddle
            DL -> cMiddle
            _  -> cNone
        Straight -> \case
            L -> cMiddle
            R -> cMiddle
            _ -> cNone
        End -> \case
            L -> cMiddle
            _ -> cNone
      where
        cNone   = Connector (False, False, False)
        cMiddle = Connector (False, True, False)
        cLeft   = Connector (True, False, False)
        cRight  = Connector (False, False, True)

instance Connects RotatedTile where
    connector (RotatedTile proto d) d'
        = connector proto (d' `dminus` d)
      where
        d1 `dminus` d2 = toEnum ((6 + fromEnum d1 - fromEnum d2) `mod` 6)

instance Connects Tile where
    connector Tile{..} = mconcat (connector <$> stack)
