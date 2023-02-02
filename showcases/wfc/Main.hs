{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
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
import Control.Monad.ST.Lazy (runST)
import Graphics.Rendering.Cairo.SVG as SVG
import qualified Graphics.Rendering.Cairo.SVG as SVG
import Control.Monad
import Data.Maybe
import Data.List.Extended
import Data.Traversable
import Debug.Trace



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

canvas :: BoundingBox
canvas = boundingBox [Vec2 (-(picWidth-cellSize)/2) (-(picHeight-cellSize)/2), Vec2 ((picWidth-cellSize)/2) ((picHeight-cellSize)/2)]

cellSize :: Num a => a
cellSize = 64

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
            drawSuperposition [ (drawTile drawProtoTile tile hex, weight) | (tile, weight) <- M.toOccurList superposition ]
        --for_ (zip (hexagonsInRange 5 hexZero) divergingTiles) $ \(h, t) -> drawTile drawProtoTile t h

drawSuperposition :: [(Render (), Int)] -> Render ()
drawSuperposition [] = pure ()
drawSuperposition actions = cairoScope $ do
    let s = sum (snd <$> actions)
    for_ actions $ \(action, weight) -> do
        let opacity = fromIntegral weight / fromIntegral s
        grouped (paintWithAlpha opacity) action

drawTile :: (ProtoTile -> Render ()) -> Tile -> Hex -> Render ()
drawTile drawProtoTile tile hex = cairoScope $ do
    let Vec2 x y = toVec2 cellSize hex
    C.translate x y
    for_ (stack tile) $ \(RotatedTile pt d) -> cairoScope $ do
        C.rotate (fromIntegral (fromEnum d) * pi/3)
        drawProtoTile pt
 


wfcSettings :: WfcSettings Hex Tile
wfcSettings = WfcSettings {..}
  where
    wfcRange = filter (\h -> toVec2 cellSize h `insideBoundingBox` canvas) $ hexagonsInRange 14 hexZero
    wfcTiles = allTiles
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
                [ maybe (isCompatible (Tile [])) (any isCompatible . M.distinctElems . getTouched) neighbour
                | (d, neighbour) <- neighbours
                , let isCompatible = connects d tile
                ]
            ]



allTiles :: M.MultiSet Tile
allTiles = M.fromOccurList $ concat
    [ (, 3000) <$> [Tile []]
    , (,   30) <$> divergingTiles
    , (,    5) <$> cornerTiles
    , (,   20) <$> straightTiles
    , (,   30) <$> endTiles
    , (,  900) <$> specialTiles
    ]

specialTiles :: [Tile]
specialTiles =
    [ Tile [RotatedTile pt d]
    | pt <- [Triple, Fork]
    , d  <- valuesOf :: [Direction]
    ]

cornerTiles :: [Tile]
cornerTiles = nubOrd
    [ Tile cornerPieces
    | d <- valuesOf :: [Direction]
    , cornerPieces <-
        [ zipWith (\posIn posOut -> RotatedTile (Corner posIn posOut) d) ins outs
        | ins  <- powerset (valuesOf :: [Pos])
        , outs <- powerset (reverse $ valuesOf :: [Pos])
        ]
    ]

divergingTiles :: [Tile]
divergingTiles = nubOrd
    [ Tile divergingPieces
    | d <- valuesOf :: [Direction]
    , divergingPieces <-
        [ zipWith3 (\i p o -> p d i o) ins pipes outs
        | ins <- powerset (valuesOf :: [Pos])
        , pipes <- powerset [flippedCorner, straight, corner]
        , outs <- powerset (reverse $ valuesOf :: [Pos])
        ]
    ]
  where
    corner        d i o = RotatedTile (Corner i o) d
    flippedCorner d i o = RotatedTile (Corner o i) (d `dminus` UL)
    straight      d i _ = RotatedTile (Straight i) d

straightTiles :: [Tile]
straightTiles =
    [ Tile straightPieces
    | d <- enumFromTo R DL
    , straightPieces <- powerset [RotatedTile (Straight pos) d | pos <- valuesOf :: [Pos]]
    ]

endTiles :: [Tile]
endTiles =
    [ Tile endPieces
    | d <- valuesOf :: [Direction]
    , endPieces <- powerset [RotatedTile (End pos) d | pos <- valuesOf :: [Pos]]
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
    = Corner Pos Pos
    | Straight Pos
    | End Pos
    | Triple
    | Fork
    deriving (Eq, Ord, Show)

-- | A, B, C = Entry point left/center/right, viewed from the outside
data Pos = A | B | C deriving (Eq, Ord, Show, Bounded, Enum)

tileSvg :: IO (ProtoTile -> Render ())
tileSvg = do
    cornerAA  <- svgNewFromFile "showcases/wfc/tiles/corner_a_a.svg"
    cornerAB  <- svgNewFromFile "showcases/wfc/tiles/corner_a_b.svg"
    cornerAC  <- svgNewFromFile "showcases/wfc/tiles/corner_a_c.svg"
    cornerBA  <- svgNewFromFile "showcases/wfc/tiles/corner_b_a.svg"
    cornerBB  <- svgNewFromFile "showcases/wfc/tiles/corner_b_b.svg"
    cornerBC  <- svgNewFromFile "showcases/wfc/tiles/corner_b_c.svg"
    cornerCA  <- svgNewFromFile "showcases/wfc/tiles/corner_c_a.svg"
    cornerCB  <- svgNewFromFile "showcases/wfc/tiles/corner_c_b.svg"
    cornerCC  <- svgNewFromFile "showcases/wfc/tiles/corner_c_c.svg"
    triple    <- svgNewFromFile "showcases/wfc/tiles/triple.svg"
    straightA <- svgNewFromFile "showcases/wfc/tiles/straight_a.svg"
    straightB <- svgNewFromFile "showcases/wfc/tiles/straight_b.svg"
    straightC <- svgNewFromFile "showcases/wfc/tiles/straight_c.svg"
    endA      <- svgNewFromFile "showcases/wfc/tiles/end_a.svg"
    endB      <- svgNewFromFile "showcases/wfc/tiles/end_b.svg"
    endC      <- svgNewFromFile "showcases/wfc/tiles/end_c.svg"
    fork      <- svgNewFromFile "showcases/wfc/tiles/fork.svg"
    pure $ \tile -> cairoScope $ do
        C.translate (-cellSize * sqrt 3 / 2) (-cellSize)
        C.scale (cellSize / 128) (cellSize / 128)
        void $ case tile of
            Corner A A  -> svgRender cornerAA
            Corner A B  -> svgRender cornerAB
            Corner A C  -> svgRender cornerAC
            Corner B A  -> svgRender cornerBA
            Corner B B  -> svgRender cornerBB
            Corner B C  -> svgRender cornerBC
            Corner C A  -> svgRender cornerCA
            Corner C B  -> svgRender cornerCB
            Corner C C  -> svgRender cornerCC
            Straight A -> svgRender straightA
            Straight B -> svgRender straightB
            Straight C -> svgRender straightC
            End A    -> svgRender endA
            End B    -> svgRender endB
            End C    -> svgRender endC
            Triple   -> svgRender triple
            Fork     -> svgRender fork

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
        Corner i o -> \case
            L  -> conn i
            DR -> conn o
            _  -> cNone
        Straight i -> \case
            L -> conn i
            R -> nnoc i
            _ -> cNone
        End i -> \case
            L -> conn i
            _ -> cNone
        Triple -> \case
            UL -> cMiddle
            R  -> cMiddle
            DL -> cMiddle
            _  -> cNone
        Fork -> \case
            L -> cMiddle
            R -> cLeft <> cRight <> cMiddle
            _ -> cNone
      where
        conn = \case
            A -> cLeft
            B -> cMiddle
            C -> cRight
        nnoc = \case
            A -> cRight
            B -> cMiddle
            C -> cLeft
        cNone   = Connector (False, False, False)
        cMiddle = Connector (False, True, False)
        cLeft   = Connector (True, False, False)
        cRight  = Connector (False, False, True)

instance Connects RotatedTile where
    connector (RotatedTile proto d) d'
        = connector proto (d' `dminus` d)

dminus :: Direction -> Direction -> Direction
d1 `dminus` d2 = toEnum ((6 + fromEnum d1 - fromEnum d2) `mod` 6)

instance Connects Tile where
    connector Tile{..} = mconcat (connector <$> stack)
