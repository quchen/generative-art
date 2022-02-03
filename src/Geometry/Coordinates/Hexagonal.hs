-- | Hexagonal coordinate systems.
--
-- Nice article about the topic: https://www.redblobgames.com/grids/hexagons/
--
-- <<docs/hexagonal/gaussian_hexagons.svg>>
module Geometry.Coordinates.Hexagonal where



import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Draw                     hiding (polygonSketch)
import qualified Draw                     as D
import           Geometry.Core            as G hiding (Polygon, pointInPolygon)
import qualified Geometry.Core            as G
import           Graphics.Rendering.Cairo as C hiding (x, y)


data Hex = Hex !Int !Int !Int
    -- This is really just a ℝ^3 with rounding occurring in every calculation,
    -- but alas, ℤ is not a field, so it isn’t a vector space.
    deriving (Eq, Ord, Show)

instance NFData Hex where
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
move :: Direction -> Int -> Hex -> Hex
move dir x (Hex q r s) = case dir of
    R  -> Hex (q+x) (r  )  (s-x)
    UR -> Hex (q+x) (r-x)  (s  )
    UL -> Hex (q  ) (r-x)  (s+x)
    L  -> Hex (q-x) (r  )  (s+x)
    DL -> Hex (q-x) (r+x)  (s  )
    DR -> Hex (q  ) (r+x)  (s-x)

hexAdd :: Hex -> Hex -> Hex
Hex q1 r1 s1 `hexAdd` Hex q2 r2 s2 = Hex (q1+q2) (r1+r2) (s1+s2)

hexSubtract :: Hex -> Hex -> Hex
Hex q1 r1 s1 `hexSubtract` Hex q2 r2 s2 = Hex (q1-q2) (r1-r2) (s1-s2)

hexTimes :: Int -> Hex -> Hex
n `hexTimes` Hex q r s = Hex (n*q) (n*r) (n*s)

hexZero :: Hex
hexZero = Hex 0 0 0

-- ^ How many steps are between two coordinates?
distance :: Hex -> Hex -> Int
distance (Hex q1 r1 s1) (Hex q2 r2 s2) = (abs (q1-q2) + abs (r1-r2) + abs (s1-s2)) `div` 2

rotateCw :: Hex -> Hex
rotateCw (Hex q r s) = Hex (-r) (-s) (-q)

rotateCcw :: Hex -> Hex
rotateCcw (Hex q r s) = Hex (-s) (-q) (-r)

-- ^ Convert a hexagonal coordinate’s center to an Euclidean 'Vec2'.
toVec2
    :: Double -- ^ Size of a hex cell (radius, side length)
    -> Hex
    -> Vec2
toVec2 size (Hex q r _) =
    let q' = fromIntegral q
        r' = fromIntegral r
        x = size * (sqrt(3)*q' + sqrt(3)/2*r')
        y = size * (                   3/2*r')
    in Vec2 x y

-- ^ Convert a Euclidean 'Vec2' to the coordiante of the hexagon it is in.
fromVec2
    :: Double -- ^ Size of a hex cell (radius, side length)
    -> Vec2
    -> Hex
fromVec2 size (Vec2 x y) =
    let q', r', s' :: Double
        q' = (sqrt(3)/3 * x - 1/3 * y) / size
        r' = (                2/3 * y) / size
        s' = -q'-r'
    in cubeRound q' r' s'

instance Semigroup Hex where
    (<>) = hexAdd

instance Monoid Hex where
    mempty = hexZero

-- Given fractional cubical coordinates, yield the hexagon the coordinate is in.
cubeRound :: Double -> Double -> Double -> Hex
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
        | qDiff > rDiff && qDiff > sDiff -> Hex (-r-s) r s
        -- r had highest diff
        | rDiff > sDiff                  -> Hex q (-q-s) s
        -- s is the only one left
        | otherwise                      -> Hex q r (-q-r)

rotateAround :: Hex -> Int -> Hex -> Hex
rotateAround center n hex =
    let hex' = hex `hexSubtract` center
        rot i x | i > 0 = rot (i-1) (rotateCw x)
                | i < 0 = rot (i+1) (rotateCcw x)
                | otherwise = x
        rotated = rot n hex'
    in rotated `hexAdd` center

-- | 'Polygon' to match a 'HexagonalCoordinate'. Useful e.g. for collision
-- checking, and of course also for painting. :-)
hexagonPoly :: Double -> Hex -> G.Polygon
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
        for_ hexagons $ \hexCoord@(Hex q r s) -> do
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
            when (hexCoord == Hex 0 0 0) $ do
                let centerHexagon = G.Polygon [corner i | i <- [0, 1, 2, 3, 4, 5]]
                D.polygonSketch (G.transform (G.scaleAround zero 0.9) centerHexagon)
                D.polygonSketch (G.transform (G.scaleAround zero 1.1) centerHexagon)
            stroke

    cairoScope $ grouped (paintWithAlpha 0.5) $ do
        for_ hexagons $ \hexCoord@(Hex q r s) -> do
            for_ [("q", q, 120), ("r", r, 240), ("s", s, 0)] $ \(name, val, angle) -> cairoScope $ do
                let center = toVec2 sideLength hexCoord
                    coord = G.transform (scaleAround center 0.2 <> G.rotateAround center (deg angle)) (center +. Vec2 0 sideLength)
                moveToVec coord
                setColor (hsva angle 1 0.7 1)
                if Hex 0 0 0 == Hex q r s
                    then cairoScope (setFontSize 14 >> showTextAligned HCenter VCenter name)
                    else showTextAligned HCenter VCenter (show val)

-- | Hexagons reachable within a number of steps from the origin.
hexagonsInRange :: Int -> Hex -> [Hex]
hexagonsInRange range center = do
    q <- [-range,-range+1..range]
    let rMin = max (-range) (-q-range)
        rMax = min range (-q+range)
    r <- [rMin, rMin+1 .. rMax]
    let s = -q-r
    pure (Hex q r s `hexAdd` center)

-- | Linear interpolation.
lerp :: Double -> Double -> Double -> Double
lerp a b t = t*a + (1-t)*b

cubeLerp :: Hex -> Hex -> Double -> Hex
cubeLerp (Hex q1 r1 s1) (Hex q2 r2 s2) t =
    let q1' = fromIntegral q1
        q2' = fromIntegral q2
        r1' = fromIntegral r1
        r2' = fromIntegral r2
        s1' = fromIntegral s1
        s2' = fromIntegral s2
    in cubeRound (lerp q1' q2' t) (lerp r1' r2' t) (lerp s1' s2' t)

line :: Hex -> Hex -> [Hex]
line start end =
    let d = distance start end
    in [ cubeLerp start end (1/fromIntegral d*fromIntegral i) | i <- [0..d] ]

-- | Ring of
ring :: Int -> Hex -> [Hex]
ring n center = do
    (startDir, walkDir) <- zip [R, UR, UL, L, DL, DR] [UL, L, DL, DR, R, UR]
    let start = move startDir n center
    [ move walkDir i start | i <- [0..n-1]]

newtype Polygon = Polygon [Hex]
    deriving (Eq, Ord, Show)

isOnEdge :: Hex -> Polygon -> Bool
isOnEdge hex (Polygon corners) =
    let edges = concat (zipWith line corners (tail (cycle corners)))
    in isJust (find (== hex) edges)

pointInPolygon :: Hex -> Polygon -> Bool
pointInPolygon hex polygon@(Polygon corners) = onEdge || inside
  where
    -- | This elimintes numerical instabilities
    onEdge = isOnEdge hex polygon

    -- This feels like cheating
    inside = G.pointInPolygon (toVec2 1 hex) (G.Polygon (map (toVec2 1) corners))

edgePoints :: Polygon -> S.Set Hex
edgePoints (Polygon corners) = S.fromList (concat (zipWith line corners (tail (cycle corners))))

polygonSketch :: Double -> Polygon -> Render ()
polygonSketch cellSize polygon =
    for_ (edgePoints polygon) $ \hex -> do
        D.polygonSketch (hexagonPoly cellSize hex)

-- | Fill all neighbours of a point, and their neighbours, and…

-- Diverges if the geometry is not closed, or the starting point is not contained
-- in it!
floodFill
    :: Hex -- ^ Starting point
    -> Set Hex
    -> Set Hex
floodFill p = go (S.singleton p)
  where
    go :: Set Hex -> Set Hex -> Set Hex
    go toVisit filled = case S.minView toVisit of
        Nothing -> filled
        Just (hex, rest) ->
            let neighbours = S.fromList (ring 1 hex)
                filled' = filled `S.union` neighbours
                toVisit' = rest `S.union` (neighbours `S.difference` filled)
            in go toVisit' filled'
