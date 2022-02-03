-- | Hexagonal coordinate systems.
--
-- Nice article about the topic: https://www.redblobgames.com/grids/hexagons/
--
-- <<docs/hexagonal/gaussian_hexagons.svg>>
module Geometry.Coordinates.Hexagonal where

import Geometry.Core as G hiding (Polygon, pointInPolygon)
import qualified Geometry.Core as G
import Graphics.Rendering.Cairo as C hiding (x,y)
import Data.Foldable
import Draw hiding (polygonSketch)
import qualified Draw as D
import Data.Maybe
import Control.Monad
import qualified Data.Set as S
import Data.Set (Set)
import Control.DeepSeq

data Cube = Cube !Int !Int !Int
    -- This is really just a ℝ^3 with rounding occurring in every calculation,
    -- but alas, ℤ is not a field, so it isn’t a vector space.
    deriving (Eq, Ord, Show)

instance NFData Cube where
    rnf _ = () -- Constructors are already strict

data Direction
    = R  -- ^ Right
    | UR -- ^ Up+right
    | UL -- ^ Up+left
    | L  -- ^ Left
    | DL -- ^ Down+left
    | DR -- ^ Down+right
    deriving (Eq, Ord, Show, Bounded, Enum)

-- | Move x steps in a direction
move :: Direction -> Int -> Cube -> Cube
move dir x (Cube q r s) = case dir of
    R  -> Cube (q+x) (r  )  (s-x)
    UR -> Cube (q+x) (r-x)  (s  )
    UL -> Cube (q  ) (r-x)  (s+x)
    L  -> Cube (q-x) (r  )  (s+x)
    DL -> Cube (q-x) (r+x)  (s  )
    DR -> Cube (q  ) (r+x)  (s-x)

hexAdd :: Cube -> Cube -> Cube
Cube q1 r1 s1 `hexAdd` Cube q2 r2 s2 = Cube (q1+q2) (r1+r2) (s1+s2)

hexSubtract :: Cube -> Cube -> Cube
Cube q1 r1 s1 `hexSubtract` Cube q2 r2 s2 = Cube (q1-q2) (r1-r2) (s1-s2)

hexTimes :: Int -> Cube -> Cube
n `hexTimes` Cube q r s = Cube (n*q) (n*r) (n*s)

hexZero :: Cube
hexZero = Cube 0 0 0

-- ^ How many steps are between two coordinates?
distance :: Cube -> Cube -> Int
distance (Cube q1 r1 s1) (Cube q2 r2 s2) = (abs (q1-q2) + abs (r1-r2) + abs (s1-s2)) `div` 2

rotateCw :: Cube -> Cube
rotateCw (Cube q r s) = Cube (-r) (-s) (-q)

rotateCcw :: Cube -> Cube
rotateCcw (Cube q r s) = Cube (-s) (-q) (-r)

-- ^ Convert a hexagonal coordinate’s center to an Euclidean 'Vec2'.
toVec2
    :: Double -- ^ Size of a hex cell (radius, side length)
    -> Cube
    -> Vec2
toVec2 size (Cube q r _) =
    let q' = fromIntegral q
        r' = fromIntegral r
        x = size * (sqrt(3)*q' + sqrt(3)/2*r')
        y = size * (                   3/2*r')
    in Vec2 x y

-- ^ Convert a Euclidean 'Vec2' to the coordiante of the hexagon it is in.
fromVec2
    :: Double -- ^ Size of a hex cell (radius, side length)
    -> Vec2
    -> Cube
fromVec2 size (Vec2 x y) =
    let q', r', s' :: Double
        q' = (sqrt(3)/3 * x - 1/3 * y) / size
        r' = (                2/3 * y) / size
        s' = -q'-r'
    in cubeRound q' r' s'

instance Semigroup Cube where
    (<>) = hexAdd

instance Monoid Cube where
    mempty = hexZero

-- Given fractional cubical coordinates, yield the hexagon the coordinate is in.
cubeRound :: Double -> Double -> Double -> Cube
cubeRound q' r' s' =
    let q,r,s :: Int
        q = round q'
        r = round r'
        s = round s'

        -- Rounding all three might violate the invariant that
        -- q+r+s=0, so we calculate the discrepancy and discard
        -- the value that was rounded the most.
        qDiff, rDiff, sDiff :: Double
        qDiff = abs (fromIntegral q - q')
        rDiff = abs (fromIntegral r - r')
        sDiff = abs (fromIntegral s - s')
    in if
        -- q had highest diff
        | qDiff > rDiff && qDiff > sDiff -> Cube (-r-s) r s
        -- r had highest diff
        | rDiff > sDiff                  -> Cube q (-q-s) s
        -- s is the only one left
        | otherwise                      -> Cube q r (-q-r)

rotateAround :: Cube -> Int -> Cube -> Cube
rotateAround center n hex =
    let hex' = hex `hexSubtract` center
        rot i x | i > 0 = rot (i-1) (rotateCw x)
                | i < 0 = rot (i+1) (rotateCcw x)
                | otherwise = x
        rotated = rot n hex'
    in rotated `hexAdd` center

-- | 'Polygon' to match a 'HexagonalCoordinate'. Useful e.g. for collision
-- checking, and of course also for painting. :-)
hexagonPoly :: Double -> Cube -> G.Polygon
hexagonPoly sideLength hex =
    let center = toVec2 sideLength hex
        oneCorner = center +. Vec2 0 sideLength
        corner n = G.transform (G.rotateAround center (deg (60*n))) oneCorner
    in G.Polygon [corner n | n <- [0..5]]


-- | Draw a hexagonal coordinate system as a helper grid, similar to
-- 'Draw.cartesianCoordinateSystem'.
hexagonalCoordinateSystem
    :: Double -- ^ Side length of a hexagon (equivalent to its radius)
    -> Int    -- ^ How many hexagons to draw in each direction
    -> Render ()
hexagonalCoordinateSystem sideLength range = do
    let hexagons = hexagonsInRange range hexZero

    cairoScope $ grouped (paintWithAlpha 0.2) $ do
        -- Variable names use Cairo coordinates, i.e. inverted y axis compared to math.

        -- First, we draw the happy path: the left-hand side of all hexagons.
        -- This will leave the ones at the top, bottom and right partially open,
        -- which we fix later.
        setLineWidth 1
        setSourceRGB 0 0 0
        for_ hexagons $ \hexCoord@(Cube q r s) -> do
            let center = toVec2 sideLength hexCoord
                bottomCorner = center +. Vec2 0 sideLength
                rotateCW degrees = G.rotateAround center (deg degrees)
                corner i = G.transform (rotateCW (i*60)) bottomCorner
            if
                -- Rightmost corner: the only full hexagon, woo!
                | q == range && s == -range -> pathSketch [corner i | i <- [0, 1, 2, 3, 4, 5]] >> closePath
                -- Upper right boundary
                | q == range                -> pathSketch [corner i | i <- [0, 1, 2, 3, 4, 5]]
                -- Lower right boundary
                | s == -range               -> pathSketch [corner i | i <- [(-2), (-1), 0, 1, 2, 3]]
                -- Upper boundary
                | r == -range               -> pathSketch [corner i | i <- [0, 1, 2, 3, 4]]
                -- Lower boundary
                | r == range                -> pathSketch [corner i | i <- [(-1), 0, 1, 2, 3]]
                | otherwise                 -> pathSketch [corner i | i <- [0, 1, 2, 3]]
            when (hexCoord == Cube 0 0 0) $ do
                let centerHexagon = G.Polygon [corner i | i <- [0, 1, 2, 3, 4, 5]]
                D.polygonSketch (G.transform (G.scaleAround zero 0.9) centerHexagon)
                D.polygonSketch (G.transform (G.scaleAround zero 1.1) centerHexagon)
            stroke

    cairoScope $ grouped (paintWithAlpha 0.5) $ do
        for_ hexagons $ \hexCoord@(Cube q r s) -> do
            for_ [("q", q, 120), ("r", r, 240), ("s", s, 0)] $ \(name, val, angle) -> cairoScope $ do
                let center = toVec2 sideLength hexCoord
                    coord = G.transform (scaleAround center 0.2 <> G.rotateAround center (deg angle)) (center +. Vec2 0 sideLength)
                moveToVec coord
                setColor (hsva angle 1 0.7 1)
                if Cube 0 0 0 == Cube q r s
                    then cairoScope (setFontSize 14 >> showTextAligned HCenter VCenter name)
                    else showTextAligned HCenter VCenter (show val)

-- | Hexagons reachable within a number of steps from the origin.
hexagonsInRange :: Int -> Cube -> [Cube]
hexagonsInRange range center = do
    q <- [-range,-range+1..range]
    let rMin = max (-range) (-q-range)
        rMax = min range (-q+range)
    r <- [rMin, rMin+1 .. rMax]
    let s = -q-r
    pure (Cube q r s `hexAdd` center)

-- | Linear interpolation.
lerp :: Double -> Double -> Double -> Double
lerp a b t = t*a + (1-t)*b

cubeLerp :: Cube -> Cube -> Double -> Cube
cubeLerp (Cube q1 r1 s1) (Cube q2 r2 s2) t =
    let q1' = fromIntegral q1
        q2' = fromIntegral q2
        r1' = fromIntegral r1
        r2' = fromIntegral r2
        s1' = fromIntegral s1
        s2' = fromIntegral s2
    in cubeRound (lerp q1' q2' t) (lerp r1' r2' t) (lerp s1' s2' t)

line :: Cube -> Cube -> [Cube]
line start end =
    let d = distance start end
    in [ cubeLerp start end (1/fromIntegral d*fromIntegral i) | i <- [0..d] ]

-- | Ring of
ring :: Int -> Cube -> [Cube]
ring n center = do
    (startDir, walkDir) <- zip [R, UR, UL, L, DL, DR] [UL, L, DL, DR, R, UR]
    let start = move startDir n center
    [ move walkDir i start | i <- [0..n-1]]

data Polygon hex = Polygon [hex]
    deriving (Eq, Ord, Show)

isOnEdge :: Cube -> Polygon Cube -> Bool
isOnEdge hex (Polygon corners) =
    let edges = concat (zipWith line corners (tail (cycle corners)))
    in isJust (find (== hex) edges)

pointInPolygon :: Cube -> Polygon Cube -> Bool
pointInPolygon hex polygon@(Polygon corners) = onEdge || inside
  where
    -- | This elimintes numerical instabilities
    onEdge = isOnEdge hex polygon

    -- This feels like cheating
    inside = G.pointInPolygon (toVec2 1 hex) (G.Polygon (map (toVec2 1) corners))

edgePoints :: Polygon Cube -> S.Set Cube
edgePoints (Polygon corners) = S.fromList (concat (zipWith line corners (tail (cycle corners))))

polygonSketch :: Double -> Polygon Cube -> Render ()
polygonSketch cellSize polygon =
    for_ (edgePoints polygon) $ \hex -> do
        D.polygonSketch (hexagonPoly cellSize hex)

-- | Fill all neighbours of a point, and their neighbours, and…

-- Diverges if the geometry is not closed, or the starting point is not contained
-- in it!
floodFill
    :: Cube -- ^ Starting point
    -> Set Cube
    -> Set Cube
floodFill p = go (S.singleton p)
  where
    go :: Set Cube -> Set Cube -> Set Cube
    go toVisit filled = case S.minView toVisit of
        Nothing -> filled
        Just (hex, rest) ->
            let neighbours = S.fromList (ring 1 hex)
                filled' = filled `S.union` neighbours
                toVisit' = rest `S.union` (neighbours `S.difference` filled)
            in go toVisit' filled'
