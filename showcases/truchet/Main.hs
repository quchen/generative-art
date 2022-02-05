module Main where

import qualified Data.Map.Strict as M
import Data.Traversable (for)
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (uniformRM, GenIO, initialize)

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
    tiling <- randomTiling gen plane

    withSurfaceAuto file scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
        Cairo.scale scaleFactor scaleFactor
        cairoScope (setColor backgroundColor >> Cairo.paint)
        for_ (strands tiling) drawStrand

colorScheme :: Int -> Color Double
colorScheme = twilight . (*0.02) . fromIntegral

backgroundColor :: Color Double
backgroundColor = blend 0.8 (colorScheme 0) white

plane :: [Hex]
plane = hexagonsInRange 15 origin
  where origin = fromVec2 cellSize (Vec2 (picWidth/2) (picHeight/2))

newtype Tile = Tile (M.Map (Direction, Int) Direction) deriving (Eq, Ord, Show)

mkTile :: [(Direction, Direction, [Int])] -> Tile
mkTile = Tile . go M.empty
  where
    go :: M.Map (Direction, Int) Direction -> [(Direction, Direction, [Int])] -> M.Map (Direction, Int) Direction
    go m [] = m
    go m ((d1, d2, is) : xs) = foldl' (addArc d1 d2) (go m xs) is
    addArc :: Direction -> Direction -> M.Map (Direction, Int) Direction -> Int -> M.Map (Direction, Int) Direction
    addArc d1 d2 m i = M.insert (d1, arcIndex d1 d2 i) d2 . M.insert (d2, arcIndex d2 d1 i) d1 $ m
    arcIndex d1 d2 i = if cyclic d1 d2 then i else 4-i

cyclic :: Direction -> Direction -> Bool
cyclic d1 d2
    | d1 == reverseDirection d2 = d1 < d2
    | otherwise = (6 + fromEnum d1 - fromEnum d2) `mod` 6 <= 3

unTile :: Tile -> [(Direction, Int, Direction)]
unTile (Tile xs) =
    [ (d1, i, d2)
    | ((d1, i), d2) <- M.toList xs
    , cyclic d1 d2
    ]

extractArc :: Tile -> Maybe ((Direction, Int, Direction), Tile)
extractArc (Tile xs)
    | M.null xs = Nothing
    | otherwise =
        let ((d1, i), d2) = M.findMin xs
        in  Just ((d1, i, d2), deleteArc (Tile xs) (d1, i, d2))

findArc :: Tile -> (Direction, Int) -> Maybe ((Direction, Int, Direction), Tile)
findArc (Tile xs) (d1, i) = fmap (\d2 -> ((d1, i, d2), deleteArc (Tile xs) (d1, i, d2))) (M.lookup (d1, i) xs)

deleteArc :: Tile -> (Direction, Int, Direction) -> Tile
deleteArc (Tile xs) (d1, i, d2) = Tile $ M.delete (d1, i) $ M.delete (d2, 4-i) xs

tiles1 :: V.Vector Tile
tiles1 = V.fromList $ allRotations =<<
    [ mkTile [(L, UR, [1..k]), (R, DL, [1..l])] | k <- [0..3], l <- [0..2], k+l == 5 ]

tiles2 :: V.Vector Tile
tiles2 = V.fromList $ allRotations =<<
    [ mkTile [(L, UL, [1..k]), (UR, R, [1..l]), (DR, DL, [1..m])] | k <- [0..3], l <- [0..3], m <- [0..3], k+l+m == 9]

tiles3 :: V.Vector Tile
tiles3 = V.fromList $ allRotations =<<
    [ mkTile [(DL, DR, [1..k]), (DR, R,  [1..l]), (R, UR, [1..m]), (UR, UL, [1..n]), (UL, L, [1..o]), (L, DL, [1..p])] | k <- [0..3], l <- [0..3], m <- [0..3], n <- [0..3], o <- [0..3], p <- [0..3], k+l == 3, l+m == 3, m+n == 3, n+o == 3, o+p == 3, p+k == 3 ]

tiles4 :: V.Vector Tile
tiles4 = V.fromList $ allRotations =<<
    [ mkTile [(L, R, [1..k]), (DL, DR, [1..l]), (UL, UR, [1..m])] | k <- [0..3], l <- [0..2], m <- [0..3], k+m <= 5, k+l+m == 7 ]

tiles5 :: V.Vector Tile
tiles5 = V.fromList $ allRotations =<<
    [ mkTile [(L, R, [1..k]), (DL, DR, [1..l]), (L, UL, [1..m]), (UL, UR, [1..n]), (UR, R, [1..m])] | k <- [0..3], l <- [2..3], m <- [0..3], n <- [0..3], if k == 0 then l == 3 else l == 2, m+n <= 3, k+m <= 3, k+n >= 4, k+n <= 5 ]

tiles :: V.Vector Tile
tiles = V.concat [ tiles1, tiles2, tiles3, tiles4, tiles5 ]

allRotations :: Tile -> [Tile]
allRotations tile = [ rotateTile i tile | i <- [0..6] ]

rotateTile :: Int -> Tile -> Tile
rotateTile n (Tile xs) = Tile $ M.fromList $ (\((d1, i), d2) -> ((rotateDirection d1, i), rotateDirection d2)) <$> M.toList xs
  where
    rotateDirection d = toEnum ((fromEnum d + n) `mod` 6)

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

strands :: Tiling -> [[(Hex, (Direction, Int, Direction))]]
strands tiling = case M.lookupMin tiling of
    Nothing -> []
    Just (startHex, t) -> case extractArc t of
        Nothing ->  strands (M.delete startHex tiling)
        Just ((d, i, d'), t') ->
            let (s, tiling') = strand tiling startHex (d, i)
                (s', tiling'') = strand tiling' startHex (d', 4-i)
            in (reverse s ++ [(startHex, (d, i, d'))] ++ s') : strands (M.insert startHex t' tiling'')

strand :: Tiling -> Hex -> (Direction, Int) -> ([(Hex, (Direction, Int, Direction))], Tiling)
strand tiling hex (d, i) = let hex' = move d 1 hex in case M.lookup hex' tiling of
    Nothing -> ([], tiling)
    Just t -> case findArc t (reverseDirection d, 4-i) of
        Nothing -> ([], tiling)
        Just ((_, _, d'), t') ->
            let (s', tiling') = strand (M.insert hex' t' tiling) hex' (d', i)
            in  ((hex', (reverseDirection d, 4-i, d')) : s', tiling')

reverseDirection :: Direction -> Direction
reverseDirection d = toEnum ((fromEnum d + 3) `mod` 6)

drawStrand :: [(Hex, (Direction, Int, Direction))] -> Cairo.Render ()
drawStrand xs = for_ xs $ \(hex, (d1, n, d2)) -> drawArc hex (d1, n, d2, length xs)

drawArc :: Hex -> (Direction, Int, Direction, Int) -> Cairo.Render ()
drawArc hex (d1, n, d2, c) = cairoScope $ do
    let i = if cyclic d1 d2 then n else 4-n
    sketchArc (fromIntegral i) d1 d2
    Cairo.setLineWidth (3/16 * cellSize)
    Cairo.setLineCap Cairo.LineCapRound
    setColor (colorScheme c)
    Cairo.stroke
  where
    center = toVec2 cellSize hex
    side d = 0.5 *. (center +. nextCenter d)
    nextCenter d = toVec2 cellSize (move d 1 hex)
    corner d d' = (center +. nextCenter d +. nextCenter d') /. 3
    [down, _lowerLeft, _upperLeft, _up, upperRight, lowerRight] = [ transform (rotate alpha) (Vec2 0 cellSize) | alpha <- deg <$> [0, 60 .. 300] ]

    sketchArc i UL DR = moveToVec ((0.5 - 0.25 * i) *. upperRight +. side UL) >> lineToVec ((0.5 - 0.25 * i) *. upperRight +. side DR)
    sketchArc i UR DL = moveToVec ((0.5 - 0.25 * i) *. lowerRight +. side DL) >> lineToVec ((0.5 - 0.25 * i) *. lowerRight +. side UR)
    sketchArc i R  L  = moveToVec ((0.5 - 0.25 * i) *. down       +. side L)  >> lineToVec ((0.5 - 0.25 * i) *. down       +. side R)

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

