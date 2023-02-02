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
        drawProtoTile Corner
        drawTile drawProtoTile (Tile [RotatedTile Corner L]) (move DR 1 hexZero)
        

data ProtoTile
    = Blank
    | Corner
    | Cross
    | Triple
    deriving (Eq, Ord, Show)

tileSvg :: IO (ProtoTile -> Render ())
tileSvg = do
    corner <- svgNewFromFile "corner.svg"
    cross <- svgNewFromFile "cross.svg"
    triple <- svgNewFromFile "triple.svg"
    pure $ \tile -> cairoScope $ do
        C.translate (-cellSize * sqrt 3 / 2) (-cellSize)
        void $ case tile of
            Blank  -> pure True
            Corner -> svgRender corner
            Cross  -> svgRender cross
            Triple -> svgRender triple

drawTile :: (ProtoTile -> Render ()) -> Tile -> Hex -> Render ()
drawTile drawProtoTile tile hex = cairoScope $ do
    let Vec2 x y = toVec2 cellSize hex
    C.translate x y
    for_ (stack tile) $ \(RotatedTile pt d) -> do
        C.rotate (fromIntegral (fromEnum d) * pi/3)
        drawProtoTile pt

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
        Blank -> const cNone
        Corner -> \case
            L  -> cMiddle
            UR -> cMiddle
            _  -> cNone
        Cross -> \case
            L  -> cLeft <> cRight
            R  -> cLeft <> cRight
            _  -> cNone
        Triple -> \case
            UL -> cMiddle
            R  -> cMiddle
            DL -> cMiddle
            _  -> cNone
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
