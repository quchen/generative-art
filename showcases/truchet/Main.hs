{-# LANGUAGE DeriveFunctor #-}
module Main where

import Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (uniformRM, GenIO, initialize)
import qualified Data.Vector as V

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

    gen <- initialize (V.fromList [123, 987])

    withSurfaceAuto file scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
        Cairo.scale scaleFactor scaleFactor
        cairoScope (setColor backgroundColor >> Cairo.paint)
        hexagonalCoordinateSystem cellSize 30 >> Cairo.newPath
        for_ plane $ \hex -> do
            tile <- liftIO $ randomTile gen
            drawTile hex tile

colorScheme :: Int -> Color Double
colorScheme = mathematica97

backgroundColor :: Color Double
backgroundColor = blend 0 (colorScheme 0) white

plane :: [Hex]
plane = hexagonsInRange 15 origin
  where origin = fromVec2 cellSize (Vec2 (picWidth/2) (picHeight/2))


newtype Tile a = Tile [((Direction, Direction), a)] deriving (Eq, Ord, Show, Functor)

tiles :: V.Vector (Tile Int)
tiles = V.fromList
    [ Tile [((L, UR), 3), ((R, DL), 2)]
    , Tile [((L, UR), 2), ((R, DL), 3)]
    , Tile [((L, DR), 3), ((R, UL), 2)]
    , Tile [((L, DR), 2), ((R, UL), 3)]
    , Tile [((L, UL), 3), ((UR, R), 3), ((DR, DL), 3)]
    , Tile [((L, DL), 3), ((DR, R), 3), ((UR, UL), 3)]
    , Tile [((L, DL), 1), ((DL, DR), 2), ((DR, R), 1), ((R, UR), 2), ((UR, UL), 1), ((UL, L), 2)]
    , Tile [((L, DL), 2), ((DL, DR), 1), ((DR, R), 2), ((R, UR), 1), ((UR, UL), 2), ((UL, L), 1)]
    ]

randomTile :: GenIO -> IO (Tile Int)
randomTile = \gen -> do
    rnd <- uniformRM (0, countTiles - 1) gen
    pure (tiles V.! rnd)
  where countTiles = V.length tiles

drawTile :: Hex -> Tile Int -> Cairo.Render ()
drawTile hex (Tile as) = for_ as $ drawArc hex

drawArc :: Hex -> ((Direction, Direction), Int) -> Cairo.Render ()
drawArc hex ((d1, d2), n) = cairoScope $ for_ [1..n] $ \i -> do
    sketchArc (fromIntegral i) d1 d2
    Cairo.setLineWidth (3/16 * cellSize)
    Cairo.setLineCap Cairo.LineCapRound
    setColor black
    Cairo.stroke
  where
    center = toVec2 cellSize hex
    side d = 0.5 *. (center +. nextCenter d)
    nextCenter d = toVec2 cellSize (move d 1 hex)
    corner d d' = (center +. nextCenter d +. nextCenter d') /. 3

    sketchArc _ L  R  = moveToVec (side L)  >> lineToVec (side R)
    sketchArc _ UL DR = moveToVec (side UL) >> lineToVec (side DR)
    sketchArc _ UR DL = moveToVec (side DL) >> lineToVec (side UR)

    sketchArc i L  UR = arcSketch (nextCenter UL) ((1 + 0.25 * i) * cellSize) (deg 30)  (deg 90)
    sketchArc i UL R  = arcSketch (nextCenter UR) ((1 + 0.25 * i) * cellSize) (deg 90)  (deg 150)
    sketchArc i UR DR = arcSketch (nextCenter R)  ((1 + 0.25 * i) * cellSize) (deg 150) (deg 210)
    sketchArc i R  DL = arcSketch (nextCenter DR) ((1 + 0.25 * i) * cellSize) (deg 210) (deg 270)
    sketchArc i DR L  = arcSketch (nextCenter DL) ((1 + 0.25 * i) * cellSize) (deg 270) (deg 330)
    sketchArc i DL UL = arcSketch (nextCenter L)  ((1 + 0.25 * i) * cellSize) (deg 330) (deg 30)

    sketchArc i L  UL = arcSketch (corner L  UL) (0.25 * i * cellSize) (deg 330) (deg 90)
    sketchArc i UL UR = arcSketch (corner UL UR) (0.25 * i * cellSize) (deg 30)  (deg 150)
    sketchArc i UR R  = arcSketch (corner UR R)  (0.25 * i * cellSize) (deg 90)  (deg 210)
    sketchArc i R  DR = arcSketch (corner R  DR) (0.25 * i * cellSize) (deg 150) (deg 270)
    sketchArc i DR DL = arcSketch (corner DR DL) (0.25 * i * cellSize) (deg 210) (deg 330)
    sketchArc i DL L  = arcSketch (corner DL L)  (0.25 * i * cellSize) (deg 270) (deg 30)

    sketchArc _ d  d' | d == d' = error ("Illegal tile " ++ show (d, d'))

    sketchArc i d  d' = sketchArc i d' d

