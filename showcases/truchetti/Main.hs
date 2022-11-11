{-# LANGUAGE DeriveFunctor #-}

module Main (main) where



import           Data.List
import           Data.List.Extended
import qualified Data.Map                 as M
import           Data.Traversable
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C
import           System.Random.MWC

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
    let scaledWidth = round (scaleFactor * picWidth)
        scaledHeight = round (scaleFactor * picHeight)

    gen <- initialize (V.fromList [123, 988])
    tiling <- indexStrands <$> randomTiling gen plane

    let drawing = do
            C.scale scaleFactor scaleFactor
            cairoScope (setColor backgroundColor >> C.paint)
            for_ (M.toList tiling) $ \(hex, tile) -> drawTile colorScheme hex tile

    render "out/penplotting-truchetti.png" scaledWidth scaledHeight drawing
    render "out/penplotting-truchetti.svg" scaledWidth scaledHeight drawing

colorScheme :: Int -> Color Double
colorScheme = twilight . (*1.21) . fromIntegral

backgroundColor :: Color Double
backgroundColor = blend 0.5 (colorScheme 0) white

plane :: [Hex]
plane = hexagonsInRange 15 origin
  where origin = fromVec2 cellSize (Vec2 (picWidth/2) (picHeight/2))


newtype Tile a = Tile [((Direction, Direction), a)] deriving (Eq, Ord, Show, Functor)

tiles :: V.Vector (Tile ())
tiles = V.fromList $ nubOrd
    [ Tile partialTile
    | [d1, d2, d3, d4, d5, d6] <- permutations allDirections
    , let fullTile = [((d1, d2), ()), ((d3, d4), ()), ((d5, d6), ())]
    , partialTile <- drop 2 $ inits fullTile
    ]
  where allDirections = [R, UR, UL, L, DL, DR]

type Tiling a = M.Map Hex (Tile a)

randomTiling :: GenIO -> [Hex] -> IO (Tiling ())
randomTiling gen coords = fmap M.fromList $ for coords $ \hex -> do
    tile <- randomTile gen
    pure (hex, tile)

randomTile :: GenIO -> IO (Tile ())
randomTile = \gen -> do
    rnd <- uniformRM (0, countTiles - 1) gen
    pure (tiles V.! rnd)
  where countTiles = V.length tiles

indexStrands :: Tiling () -> Tiling Int
indexStrands = \tiling -> goStrands 0 (strands tiling) (fmap (fmap (const 0)) tiling)
  where
    goStrands _ [] t = t
    goStrands n (s:ss) t = goStrand n s (goStrands (n+1) ss t)

    goStrand _ [] t = t
    goStrand n ((hex, (directions, _)) : xs) t = M.adjust (updateIndex n directions) hex (goStrand n xs t)

    updateIndex n (d1, d2) (Tile xs) = Tile $ flip fmap xs $ \(directions', n') -> if
        | (d1, d2) == directions' -> (directions', n)
        | (d2, d1) == directions' -> (directions', n)
        | otherwise -> (directions', n')


strands :: Tiling a -> [[(Hex, ((Direction, Direction), a))]]
strands tiling = case M.lookupMin tiling of
    Nothing -> []
    Just (startHex, tile) -> case tile of
        Tile [] -> strands (M.delete startHex tiling)
        Tile (((d, d'), a):ts) ->
            let (s, tiling') = strand tiling startHex d
                (s', tiling'') = strand tiling' startHex d'
            in (reverse s ++ [(startHex, ((d, d'), a))] ++ s') : strands (M.insert startHex (Tile ts) tiling'')

strand :: Tiling a -> Hex -> Direction -> ([(Hex, ((Direction, Direction), a))], Tiling a)
strand tiling hex d = let hex' = move d 1 hex in case M.lookup hex' tiling of
    Nothing -> ([], tiling)
    Just (Tile ds)
        | ([((_, d'), a)], ds') <- partition ((== reverseDirection d) . fst . fst) ds ->
            let (s', tiling') = strand (M.insert hex' (Tile ds') tiling) hex' d'
            in  ((hex', ((reverseDirection d, d'), a)) : s', tiling')
        | ([((d', _), a)], ds') <- partition ((== reverseDirection d) . snd . fst) ds ->
            let (s', tiling') = strand (M.insert hex' (Tile ds') tiling) hex' d'
            in  ((hex', ((reverseDirection d, d'), a)) : s', tiling')
        | otherwise -> ([], tiling)

reverseDirection :: Direction -> Direction
reverseDirection d = toEnum ((fromEnum d + 3) `mod` 6)

drawTile :: (Int -> Color Double) -> Hex -> Tile Int -> C.Render ()
drawTile colors hex (Tile as) = for_ as $ drawArc colors hex

drawArc :: (Int -> Color Double) -> Hex -> ((Direction, Direction), Int) -> C.Render ()
drawArc colors hex ((d1, d2), i) = cairoScope $ do
    sketchArc d1 d2
    C.setLineWidth (cellSize / 2)
    setColor (backgroundColor `withOpacity` 0.7)
    C.stroke
    sketchArc d1 d2
    C.setLineWidth (3/8 * cellSize)
    C.setLineCap C.LineCapRound
    setColor (colors i)
    C.stroke
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

    sketchArc d  d' | d == d' = error ("Illegal tile " ++ show (d, d'))

    sketchArc d  d' = sketchArc d' d
