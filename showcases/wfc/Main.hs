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
import Draw hiding (Cross)
import Draw.Grid
import Geometry                     as G
import Geometry.Coordinates.Hexagonal
import Geometry.Algorithms.WaveFunctionCollapse
import Control.Monad.ST (runST)
import Graphics.Rendering.Cairo.SVG as SVG
import qualified Graphics.Rendering.Cairo.SVG as SVG
import Control.Monad



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

cellSize :: Num a => a
cellSize = 128

main :: IO ()
main = do
    drawProtoTile <- tileSvg
    render "showcases/wfc/template.png" 1440 1440 $ do
        cairoScope (setColor white >> paint)
        C.translate 720 720
        hexagonalCoordinateSystem cellSize (720 `div` cellSize)
        for_ (zip allTiles (hexagonsInRange 2 hexZero)) $ \(tile, hex) -> cairoScope $ do
            drawTile drawProtoTile tile hex

drawTile :: (ProtoTile -> Render ()) -> Tile -> Hex -> Render ()
drawTile drawProtoTile tile hex = cairoScope $ do
    let Vec2 x y = toVec2 cellSize hex
    C.translate x y
    for_ (stack tile) $ \(RotatedTile pt d) -> do
        C.rotate (fromIntegral (fromEnum d) * pi/3)
        drawProtoTile pt
        

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
            UR -> cMiddle
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
