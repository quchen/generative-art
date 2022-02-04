{-# LANGUAGE DeriveFunctor #-}
module Main where

import qualified Graphics.Rendering.Cairo as Cairo
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
            tile <- Cairo.liftIO $ randomTile gen
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
tiles = V.fromList $ concat
    --[ [ Tile [((L, UR), k), ((R, DL), l)] | k <- [0..3], l <- [0..2], k+l == 5 ]
    --, [ Tile [((L, DR), k), ((R, UL), l)] | k <- [0..3], l <- [0..2], k+l == 5 ]
    [ [ Tile [((L, UL), k), ((UR, R), l), ((DR, DL), m)] | k <- [0..3], l <- [0..3], m <- [0..3], k+l+m == 9]
    , [ Tile [((L, DL), k), ((DR, R), l), ((UR, UL), m)] | k <- [0..3], l <- [0..3], m <- [0..3], k+l+m == 9]
    --[ [ Tile [((DL, DR), k), ((DR, R),  l), ((R, UR), m), ((UR, UL), n), ((UL, L),  o), ((L, DL), p)] | k <- [0..2], l <- [0..2], m <- [0..2], n <- [0..2], o <- [0..2], p <- [0..2], k+l <= 3, l+m <= 3, m+n <= 3, n+o <= 3, o+p <= 3, p+k <= 3, k+l+m+n+o+p >= 9 ]
    --, [ Tile [((L,  DL), k), ((DL, DR), l), ((DR, R), m), ((R, UR),  n), ((UR, UL), o), ((UL, L), p)] | k <- [0..2], l <- [0..2], m <- [0..2], n <- [0..2], o <- [0..2], p <- [0..2], k+l <= 3, l+m <= 3, m+n <= 3, n+o <= 3, o+p <= 3, p+k <= 3, k+l+m+n+o+p >= 9 ]
    --, [ Tile [((L, R),   k), ((UL, UR), l), ((DL, DR), m)] | k <- [0..3], l <- [0..2], m <- [0..3], k+m <= 5, k+l+m == 7 ]
    --, [ Tile [((R, L),   k), ((DL, DR), l), ((UL, UR), m)] | k <- [0..3], l <- [0..2], m <- [0..3], k+m <= 5, k+l+m == 7 ]
    --, [ Tile [((UL, DR), k), ((UR, R),  l), ((L, DL),  m)] | k <- [0..3], l <- [0..2], m <- [0..3], k+m <= 5, k+l+m == 7 ]
    --, [ Tile [((DR, UL), k), ((L, DL),  l), ((UR, R),  m)] | k <- [0..3], l <- [0..2], m <- [0..3], k+m <= 5, k+l+m == 7 ]
    --, [ Tile [((DL, UR), k), ((UL, L),  l), ((R, DR),  m)] | k <- [0..3], l <- [0..2], m <- [0..3], k+m <= 5, k+l+m == 7 ]
    --, [ Tile [((UR, DL), k), ((R, DR),  l), ((UL, L),  m)] | k <- [0..3], l <- [0..2], m <- [0..3], k+m <= 5, k+l+m == 7 ]
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
    [down, lowerLeft, upperLeft, up, upperRight, lowerRight] = [ transform (rotate alpha) (Vec2 0 cellSize) | alpha <- deg <$> [0, 60 .. 300] ]

    sketchArc i L  R  = moveToVec ((0.5 - 0.25 * i) *. up         +. side L)  >> lineToVec ((0.5 - 0.25 * i) *. up         +. side R)
    sketchArc i UL DR = moveToVec ((0.5 - 0.25 * i) *. upperRight +. side UL) >> lineToVec ((0.5 - 0.25 * i) *. upperRight +. side DR)
    sketchArc i UR DL = moveToVec ((0.5 - 0.25 * i) *. lowerRight +. side DL) >> lineToVec ((0.5 - 0.25 * i) *. lowerRight +. side UR)
    sketchArc i R  L  = moveToVec ((0.5 - 0.25 * i) *. down       +. side L)  >> lineToVec ((0.5 - 0.25 * i) *. down       +. side R)
    sketchArc i DR UL = moveToVec ((0.5 - 0.25 * i) *. lowerLeft  +. side UL) >> lineToVec ((0.5 - 0.25 * i) *. lowerLeft  +. side DR)
    sketchArc i DL UR = moveToVec ((0.5 - 0.25 * i) *. upperLeft  +. side DL) >> lineToVec ((0.5 - 0.25 * i) *. upperLeft  +. side UR)

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

