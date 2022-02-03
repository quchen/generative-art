module Main where

import Data.Traversable (for)
import Data.List (permutations, inits, nub)
import Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (create, uniformRM, GenIO)

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

    withSurfaceAuto file scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
        Cairo.scale scaleFactor scaleFactor
        cairoScope (setColor white >> Cairo.paint)
        for_ plane $ \hex -> do
            tile <- Cairo.liftIO (randomTile gen)
            drawTile hex tile



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

randomTile :: GenIO -> IO Tile
randomTile = \gen -> do
    rnd <- uniformRM (0, countTiles - 1) gen
    pure (tiles !! rnd)
  where countTiles = length tiles

drawTile :: Hex -> Tile -> Cairo.Render ()
drawTile hex (Tile ds) = for_ ds $ \(d1, d2) -> drawArc d1 d2
  where
    center = toVec2 cellSize hex
    side d = 0.5 *. (center +. nextCenter d)
    nextCenter d = toVec2 cellSize (move d 1 hex)
    corner d d' = (center +. nextCenter d +. nextCenter d') /. 3

    drawArc d d' = cairoScope $ do
        sketchArc d d'
        Cairo.setLineWidth (cellSize / 2)
        setColor white
        Cairo.stroke
        sketchArc d d'
        Cairo.setLineWidth (3/8 * cellSize)
        Cairo.setLineCap Cairo.LineCapRound
        setColor black
        Cairo.stroke

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

