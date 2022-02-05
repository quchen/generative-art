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

tiles1 :: V.Vector (Tile Int)
tiles1 = V.fromList $ allRotations =<<
    [ Tile [((L, UR), k), ((R, DL), l)] | k <- [0..3], l <- [0..2], k+l == 5 ]

tiles2 :: V.Vector (Tile Int)
tiles2 = V.fromList $ allRotations =<<
    [ Tile [((L, UL), k), ((UR, R), l), ((DR, DL), m)] | k <- [0..3], l <- [0..3], m <- [0..3], k+l+m == 9]

tiles3 :: V.Vector (Tile Int)
tiles3 = V.fromList $ allRotations =<<
    [ Tile [((DL, DR), k), ((DR, R),  l), ((R, UR), m), ((UR, UL), n), ((UL, L),  o), ((L, DL), p)] | k <- [0..3], l <- [0..3], m <- [0..3], n <- [0..3], o <- [0..3], p <- [0..3], k+l == 3, l+m == 3, m+n == 3, n+o == 3, o+p == 3, p+k == 3 ]

tiles4 :: V.Vector (Tile Int)
tiles4 = V.fromList $ allRotations =<<
    [ Tile [((L, R), k), ((UL, UR), l), ((DL, DR), m)] | k <- [0..3], l <- [0..2], m <- [0..3], k+m <= 5, k+l+m == 7 ]

tiles5 :: V.Vector (Tile Int)
tiles5 = V.fromList $ allRotations =<<
    [ Tile [((L, R), k), ((UL, UR), l), ((L, DL), m), ((DL, DR), n), ((DR, R), m)] | k <- [0..3], l <- [2..3], m <- [0..3], n <- [0..3], if k == 0 then l == 3 else l == 2, m+n <= 3, k+m <= 3, k+n >= 4, k+n <= 5 ]

tiles :: V.Vector (Tile Int)
tiles = V.concat [tiles1, tiles2, tiles3, tiles4, tiles5]

allRotations :: Tile a -> [Tile a]
allRotations tile = [ rotateTile i tile | i <- [0..6] ]

rotateTile :: Int -> Tile a -> Tile a
rotateTile n (Tile xs) = Tile (fmap (\((d1, d2), a) -> ((rotateDirection d1, rotateDirection d2), a)) xs)
  where
    rotateDirection d = toEnum ((fromEnum d + n) `mod` 6)

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
    setColor (colorScheme (i `mod` 2))
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

