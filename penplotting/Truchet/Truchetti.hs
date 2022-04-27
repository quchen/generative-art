{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where



import           Data.List
import           Data.List.Extended
import qualified Data.Map                 as M
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector              as V
import           Control.Monad
import qualified Graphics.Rendering.Cairo as C
import           System.Random.MWC

import Arc
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
            for_ (strands tiling) $ drawStrand . V.toList

    render "out/penplotting-truchetti.png" picWidth picHeight drawing
    render "out/penplotting-truchetti.svg" picWidth picHeight drawing

plane :: [Hex]
plane = hexagonsInRange 15 origin
  where origin = fromVec2 cellSize (Vec2 (picWidth/2) (picHeight/2))


newtype Tile = Tile (M.Map Direction Direction) deriving (Eq, Ord, Show)

tiles :: V.Vector Tile
tiles = V.fromList $ nubOrd
    [ Tile (M.fromList (partialTile ++ (swap <$> partialTile)))
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

extractArc :: Tile -> Maybe ((Direction, Direction), Tile)
extractArc (Tile xs)
    | M.null xs = Nothing
    | otherwise =
        let (d1, d2) = M.findMin xs
        in  Just ((d1, d2), deleteArc (Tile xs) (d1, d2))

findArc :: Tile -> Direction -> Maybe ((Direction, Direction), Tile)
findArc (Tile xs) d1 = fmap (\d2 -> ((d1, d2), deleteArc (Tile xs) (d1, d2))) (M.lookup d1 xs)

deleteArc :: Tile -> (Direction, Direction) -> Tile
deleteArc (Tile xs) (d1, d2) = Tile $ M.delete d1 $ M.delete d2 xs

strands :: Tiling -> [V.Vector (Hex, (Direction, Direction))]
strands tiling = case M.lookupMin tiling of
    Nothing -> []
    Just (startHex, t) -> case extractArc t of
        Nothing ->  strands (M.delete startHex tiling)
        Just ((d, d'), t') ->
            let tiling' = M.insert startHex t' tiling
                (s, tiling'') = strand tiling' startHex d
                (s', tiling''') = strand tiling'' startHex d'
            in V.fromList (reverseStrand s ++ [(startHex, (d, d'))] ++ s') : strands tiling'''

strand :: Tiling -> Hex -> Direction -> ([(Hex, (Direction, Direction))], Tiling)
strand tiling hex d = let hex' = move d 1 hex in case M.lookup hex' tiling of
    Nothing -> ([], tiling)
    Just t -> case findArc t (reverseDirection d) of
        Nothing -> ([], tiling)
        Just ((_, d'), t') ->
            let (s', tiling') = strand (M.insert hex' t' tiling) hex' d'
            in  ((hex', (reverseDirection d, d')) : s', tiling')

reverseStrand :: [(Hex, (Direction, Direction))] -> [(Hex, (Direction, Direction))]
reverseStrand = fmap (\(h, (d1, d2)) -> (h, (d2, d1))) . reverse

reverseDirection :: Direction -> Direction
reverseDirection d = toEnum ((fromEnum d + 3) `mod` 6)

toArc :: Hex -> (Direction, Double, Direction) -> Arc
toArc hex (d1, n, d2) = sketchArc n' d1 d2
  where
    n' = if cyclic d1 d2 then n else 1-n
    center = toVec2 cellSize hex
    side d = 0.5 *. (center +. nextCenter d)
    nextCenter d = toVec2 cellSize (move d 1 hex)
    corner d d' = (center +. nextCenter d +. nextCenter d') /. 3
    [down, _lowerLeft, _upperLeft, _up, upperRight, lowerRight] = [ transform (rotate alpha) (Vec2 0 cellSize) | alpha <- deg <$> [0, 60 .. 300] ]

    sketchArc i DR UL = straight ((0.5 - i) *. upperRight +. side DR) ((0.5 - i) *. upperRight +. side UL)
    sketchArc i UR DL = straight ((0.5 - i) *. lowerRight +. side UR) ((0.5 - i) *. lowerRight +. side DL)
    sketchArc i R  L  = straight ((0.5 - i) *. down       +. side R)  ((0.5 - i) *. down       +. side L)
    sketchArc i UL DR = straight ((0.5 - i) *. upperRight +. side UL) ((0.5 - i) *. upperRight +. side DR)
    sketchArc i DL UR = straight ((0.5 - i) *. lowerRight +. side DL) ((0.5 - i) *. lowerRight +. side UR)
    sketchArc i L  R  = straight ((0.5 - i) *. down       +. side L)  ((0.5 - i) *. down       +. side R)

    sketchArc i UR L  = ccwArc (nextCenter UL) ((1 + i) * cellSize) (deg 30)  (deg 90)
    sketchArc i R  UL = ccwArc (nextCenter UR) ((1 + i) * cellSize) (deg 90)  (deg 150)
    sketchArc i DR UR = ccwArc (nextCenter R)  ((1 + i) * cellSize) (deg 150) (deg 210)
    sketchArc i DL R  = ccwArc (nextCenter DR) ((1 + i) * cellSize) (deg 210) (deg 270)
    sketchArc i L  DR = ccwArc (nextCenter DL) ((1 + i) * cellSize) (deg 270) (deg 330)
    sketchArc i UL DL = ccwArc (nextCenter L)  ((1 + i) * cellSize) (deg 330) (deg 30)
    sketchArc i L  UR = cwArc (nextCenter UL) ((1 + i) * cellSize) (deg 90)  (deg 30)
    sketchArc i UL R  = cwArc (nextCenter UR) ((1 + i) * cellSize) (deg 150) (deg 90)
    sketchArc i UR DR = cwArc (nextCenter R)  ((1 + i) * cellSize) (deg 210) (deg 150)
    sketchArc i R  DL = cwArc (nextCenter DR) ((1 + i) * cellSize) (deg 270) (deg 210)
    sketchArc i DR L  = cwArc (nextCenter DL) ((1 + i) * cellSize) (deg 330) (deg 270)
    sketchArc i DL UL = cwArc (nextCenter L)  ((1 + i) * cellSize) (deg 30)  (deg 330)

    sketchArc i UL L  = ccwArc (corner L  UL) (i * cellSize) (deg 330) (deg 90)
    sketchArc i UR UL = ccwArc (corner UL UR) (i * cellSize) (deg 30)  (deg 150)
    sketchArc i R  UR = ccwArc (corner UR R)  (i * cellSize) (deg 90)  (deg 210)
    sketchArc i DR R  = ccwArc (corner R  DR) (i * cellSize) (deg 150) (deg 270)
    sketchArc i DL DR = ccwArc (corner DR DL) (i * cellSize) (deg 210) (deg 330)
    sketchArc i L  DL = ccwArc (corner DL L)  (i * cellSize) (deg 270) (deg 30)
    sketchArc i L  UL = cwArc (corner L  UL) (i * cellSize) (deg 90)  (deg 330)
    sketchArc i UL UR = cwArc (corner UL UR) (i * cellSize) (deg 150) (deg 30)
    sketchArc i UR R  = cwArc (corner UR R)  (i * cellSize) (deg 210) (deg 90)
    sketchArc i R  DR = cwArc (corner R  DR) (i * cellSize) (deg 270) (deg 150)
    sketchArc i DR DL = cwArc (corner DR DL) (i * cellSize) (deg 330) (deg 210)
    sketchArc i DL L  = cwArc (corner DL L)  (i * cellSize) (deg 30)  (deg 270)

    sketchArc _ d  d' = error ("Illegal tile " ++ show (d, d'))

cyclic :: Direction -> Direction -> Bool
cyclic d1 d2
    | d1 == reverseDirection d2 = d1 < d2
    | otherwise = (6 + fromEnum d1 - fromEnum d2) `mod` 6 <= 3

drawStrand :: [(Hex, (Direction, Direction))] -> C.Render ()
drawStrand xs = cairoScope $ do
    let arcAtThreeEights (hex, (d1, d2)) = toArc hex (d1, 3/8, d2)
        arcsThere = fmap arcAtThreeEights xs
        arcsBack  = fmap arcAtThreeEights (reverseStrand xs)
        (p1, _) = arcStartEnd (head arcsThere)
        (_, p2) = arcStartEnd (last arcsBack)
        (_, p3) = arcStartEnd (last arcsThere)
        (p4, _) = arcStartEnd (head arcsBack)
        pathClosed = norm (p1 -. p3) < 0.1
    C.setLineWidth (1/8 * cellSize)
    setColor white
    for_ arcsThere sketch
    unless pathClosed $ sketch (CcwArc (0.5 *. (p3 +. p4)) p3 p4)
    C.stroke
    for_ arcsBack sketch
    unless pathClosed $ sketch (CcwArc (0.5 *. (p1 +. p2)) p2 p1)
    C.stroke
