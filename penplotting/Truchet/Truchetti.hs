{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where



import           Data.List
import           Data.List.Extended
import qualified Data.Map                 as M
import           Data.Traversable
import qualified Data.Vector              as V
import qualified Graphics.Rendering.Cairo as C
import           System.Random.MWC

import Draw
import Geometry
import Geometry.Coordinates.Hexagonal



picWidth, picHeight :: Num a => a
picWidth = 700
picHeight = 500

cellSize :: Num a => a
cellSize = 20

main :: IO ()
main = do
    gen <- initialize (V.fromList [123, 988])
    tiling <- randomTiling gen plane

    let drawing = do
            cairoScope (setColor black >> C.paint)
            for_ (M.toList tiling) $ \(hex, tile) -> drawTile hex tile

    render "out/penplotting-truchetti.png" picWidth picHeight drawing
    render "out/penplotting-truchetti.svg" picWidth picHeight drawing

plane :: [Hex]
plane = hexagonsInRange 15 origin
  where origin = fromVec2 cellSize (Vec2 (picWidth/2) (picHeight/2))


newtype Tile = Tile [(Direction, Direction)] deriving (Eq, Ord, Show)

tiles :: V.Vector Tile
tiles = V.fromList $ nubOrd
    [ Tile partialTile
    | [d1, d2, d3, d4, d5, d6] <- permutations allDirections
    , let fullTile = [(d1, d2), (d3, d4), (d5, d6)]
    , partialTile <- drop 2 $ inits fullTile
    ]
  where allDirections = [R, UR, UL, L, DL, DR]

type Tiling = M.Map Hex Tile

randomTiling :: GenIO -> [Hex] -> IO Tiling
randomTiling gen coords = fmap M.fromList $ for coords $ \hex -> do
    tile <- randomTile gen
    pure (hex, tile)

randomTile :: GenIO -> IO Tile
randomTile = \gen -> do
    rnd <- uniformRM (0, countTiles - 1) gen
    pure (tiles V.! rnd)
  where countTiles = V.length tiles

drawTile :: Hex -> Tile -> C.Render ()
drawTile hex (Tile as) = for_ as $ drawArc hex

data Arc = Arc
    { _arcCenter :: Vec2
    , _arcRadius :: Double
    , _arcStartAngle :: Angle
    , _arcEndAngle :: Angle
    }

instance Sketch Arc where
    sketch Arc{..} = Draw.arcSketch _arcCenter _arcRadius _arcStartAngle _arcEndAngle

data ArcSketch = forall a. Sketch a => ArcSketch a

instance Sketch ArcSketch where
    sketch (ArcSketch s) = sketch s

arc :: Hex -> (Direction, Direction) -> [ArcSketch]
arc hex (d1, d2) = [sketchArc d1 d2]
  where
    center = toVec2 cellSize hex
    side d = 0.5 *. (center +. nextCenter d)
    nextCenter d = toVec2 cellSize (move d 1 hex)
    corner d d' = (center +. nextCenter d +. nextCenter d') /. 3

    sketchArc :: Direction -> Direction -> ArcSketch

    sketchArc L  R  = ArcSketch $ Line (side L)  (side R)
    sketchArc UL DR = ArcSketch $ Line (side UL) (side DR)
    sketchArc UR DL = ArcSketch $ Line (side DL) (side UR)

    sketchArc L  UR = ArcSketch $ Arc (nextCenter UL) (1.5 * cellSize) (deg 30)  (deg 90)
    sketchArc UL R  = ArcSketch $ Arc (nextCenter UR) (1.5 * cellSize) (deg 90)  (deg 150)
    sketchArc UR DR = ArcSketch $ Arc (nextCenter R)  (1.5 * cellSize) (deg 150) (deg 210)
    sketchArc R  DL = ArcSketch $ Arc (nextCenter DR) (1.5 * cellSize) (deg 210) (deg 270)
    sketchArc DR L  = ArcSketch $ Arc (nextCenter DL) (1.5 * cellSize) (deg 270) (deg 330)
    sketchArc DL UL = ArcSketch $ Arc (nextCenter L)  (1.5 * cellSize) (deg 330) (deg 30)

    sketchArc L  UL = ArcSketch $ Arc (corner L  UL) (0.5 * cellSize) (deg 330) (deg 90)
    sketchArc UL UR = ArcSketch $ Arc (corner UL UR) (0.5 * cellSize) (deg 30)  (deg 150)
    sketchArc UR R  = ArcSketch $ Arc (corner UR R)  (0.5 * cellSize) (deg 90)  (deg 210)
    sketchArc R  DR = ArcSketch $ Arc (corner R  DR) (0.5 * cellSize) (deg 150) (deg 270)
    sketchArc DR DL = ArcSketch $ Arc (corner DR DL) (0.5 * cellSize) (deg 210) (deg 330)
    sketchArc DL L  = ArcSketch $ Arc (corner DL L)  (0.5 * cellSize) (deg 270) (deg 30)

    sketchArc d  d' | d == d' = error ("Illegal tile " ++ show (d, d'))

    sketchArc d  d' = sketchArc d' d

drawArc :: Hex -> (Direction, Direction) -> C.Render ()
drawArc hex (d1, d2) = cairoScope $ do
    for_ (arc hex (d1, d2)) sketch
    C.setLineWidth (3/8 * cellSize)
    C.setLineCap C.LineCapRound
    setColor white
    C.stroke
    for_ (arc hex (d1, d2)) sketch
    C.setLineWidth (1/8 * cellSize)
    C.setLineCap C.LineCapRound
    setColor black
    C.stroke
