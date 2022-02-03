module Main where

import Data.Traversable (for)
import Data.List (permutations, inits, nub, partition)
import Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (create, uniformRM, GenIO)
import qualified Data.Map.Strict as M

import Draw
import Geometry
import Geometry.Coordinates.Hexagonal



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

scaleFactor :: Double
scaleFactor = 0.5

cellSize :: Num a => a
cellSize = 64

main :: IO ()
main = do
    let file = "out/truchet.png"
        scaledWidth = round (scaleFactor * picWidth)
        scaledHeight = round (scaleFactor * picHeight)

    gen <- create
    tiling <- randomTiling gen plane

    withSurfaceAuto file scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
        Cairo.scale scaleFactor scaleFactor
        cairoScope (setColor white >> Cairo.paint)
        for_ (strands tiling) drawStrand



plane :: [Hex]
plane = hexagonsInRange 15 origin
  where origin = fromVec2 cellSize (Vec2 (picWidth/2) (picHeight/2))


newtype Tile = Tile [(Direction, Direction)] deriving (Eq)

tiles :: [Tile]
tiles = nub
    [ Tile partialTile
    | [d1, d2, d3, d4, d5, d6] <- permutations allDirections
    , let fullTile = [(d1, d2), (d3, d4), (d5, d6)]
    , partialTile <- inits fullTile
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
    pure (tiles !! rnd)
  where countTiles = length tiles

strands :: Tiling -> [[(Hex, (Direction, Direction))]]
strands tiling = case M.lookupMin tiling of
    Nothing -> []
    Just (startHex, tile) -> case tile of
        Tile [] -> strands (M.delete startHex tiling)
        Tile ((d, d'):ts) ->
            let (s, tiling') = strand tiling startHex d
                (s', tiling'') = strand tiling' startHex d'
            in (reverse s ++ [(startHex, (d, d'))] ++ s') : strands (M.insert startHex (Tile ts) tiling'')

strand :: Tiling -> Hex -> Direction -> ([(Hex, (Direction, Direction))], Tiling)
strand tiling hex d = let hex' = move d 1 hex in case M.lookup hex' tiling of
    Nothing -> ([], tiling)
    Just (Tile ds)
        | ([(_, d')], ds') <- partition ((== d) . fst) ds ->
            let (s', tiling') = strand (M.insert hex' (Tile ds') tiling) hex' d'
            in  ((hex', (d, d')) : s', tiling')
        | ([(d', _)], ds') <- partition ((== d) . snd) ds ->
            let (s', tiling') = strand (M.insert hex' (Tile ds') tiling) hex' d'
            in  ((hex', (d, d')) : s', tiling')
        | otherwise -> ([], tiling)

drawStrand :: [(Hex, (Direction, Direction))] -> Render ()
drawStrand [] = pure ()
drawStrand ((hex, (d0, d1)) : rest) = drawArc hex (d0, d1) >> drawStrand rest

drawArc :: Hex -> (Direction, Direction) -> Cairo.Render ()
drawArc hex (d1, d2) = cairoScope $ do
    sketchArc d1 d2
    Cairo.setLineWidth (cellSize / 2)
    setColor white
    Cairo.stroke
    sketchArc d1 d2
    Cairo.setLineWidth (3/8 * cellSize)
    Cairo.setLineCap Cairo.LineCapRound
    setColor black
    Cairo.stroke
  where
    center = toVec2 cellSize hex
    side d = 0.5 *. (center +. nextCenter d)
    nextCenter d = toVec2 cellSize (move d 1 hex)
    corner d d' = (center +. nextCenter d +. nextCenter d') /. 3


    sketchArc L  R  = moveToVec (side L)  >> lineToVec (side R)
    sketchArc UL DR = moveToVec (side UL) >> lineToVec (side DR)
    sketchArc UR DL = moveToVec (side DL) >> lineToVec (side UR)

    sketchArc L  UR = arcSketch (nextCenter UL) (1.5 * cellSize) (deg 30)  (deg 90)
    sketchArc UL R  = arcSketch (nextCenter UR) (1.5 * cellSize) (deg 90)  (deg 150)
    sketchArc UR DR = arcSketch (nextCenter R)  (1.5 * cellSize) (deg 150) (deg 210)
    sketchArc R  DL = arcSketch (nextCenter DR) (1.5 * cellSize) (deg 210) (deg 270)
    sketchArc DR L  = arcSketch (nextCenter DL) (1.5 * cellSize) (deg 270) (deg 330)
    sketchArc DL UL = arcSketch (nextCenter L)  (1.5 * cellSize) (deg 330) (deg 30)

    sketchArc L  UL = arcSketch (corner L  UL) (0.5 * cellSize) (deg 330) (deg 90)
    sketchArc UL UR = arcSketch (corner UL UR) (0.5 * cellSize) (deg 30)  (deg 150)
    sketchArc UR R  = arcSketch (corner UR R)  (0.5 * cellSize) (deg 90)  (deg 210)
    sketchArc R  DR = arcSketch (corner R  DR) (0.5 * cellSize) (deg 150) (deg 270)
    sketchArc DR DL = arcSketch (corner DR DL) (0.5 * cellSize) (deg 210) (deg 330)
    sketchArc DL L  = arcSketch (corner DL L)  (0.5 * cellSize) (deg 270) (deg 30)

    sketchArc d d' = sketchArc d' d

