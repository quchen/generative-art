-- | Hexagonal coordinate systems.
--
-- Nice article about the topic: https://www.redblobgames.com/grids/hexagons/
module Geometry.Coordinates.Hexagonal where

import Geometry.Core as G hiding (Polygon, pointInPolygon)
import qualified Geometry.Core as G
import Graphics.Rendering.Cairo as C hiding (x,y)
import Data.Foldable
import Draw
import qualified Data.Set as S
import Data.Maybe
import Data.List
import Control.Monad

data Cube = Cube !Int !Int !Int
    deriving (Eq, Ord, Show)

data Axial = Axial !Int !Int
    deriving (Eq, Ord, Show)

cubicalToAxial :: Cube -> Axial
cubicalToAxial (Cube q r _s) = Axial q r

axialToCubical :: Axial -> Cube
axialToCubical (Axial q r) = Cube q r (-q-r)

data Direction
    = R  -- ^ Right
    | UR -- ^ Up+right
    | UL -- ^ Up+left
    | L  -- ^ Left
    | DL -- ^ Down+left
    | DR -- ^ Down+right
    deriving (Eq, Ord, Show)

class HexagonalCoordinate hex where
    -- ^ Move x steps in a direction
    move :: Direction -> Int -> hex -> hex

    -- | This is really just a ℝ^3 with rounding occurring in every calculation,
    -- but alas, ℤ is not a field, so it isn’t a vector space.
    hexAdd      :: hex -> hex -> hex
    hexSubtract :: hex -> hex -> hex
    hexTimes    :: Int -> hex -> hex
    hexZero     :: hex

    -- ^ How many steps are between two coordinates?
    distance :: hex -> hex -> Int

    rotateCw :: hex -> hex
    rotateCcw :: hex -> hex

    -- ^ Convert a hexagonal coordinate’s center to an Euclidean 'Vec2'.
    toVec2
        :: Double -- ^ Size of a hex cell (radius, side length)
        -> hex
        -> Vec2

    -- ^ Convert a Euclidean 'Vec2' to the coordiante of the hexagon it is in.
    fromVec2
        :: Double -- ^ Size of a hex cell (radius, side length)
        -> Vec2
        -> hex

instance HexagonalCoordinate Cube where
    move dir x (Cube q r s) = case dir of
        R  -> Cube (q+x) (r  )  (s-x)
        UR -> Cube (q+x) (r-x)  (s  )
        UL -> Cube (q  ) (r-x)  (s+x)
        L  -> Cube (q-x) (r  )  (s+x)
        DL -> Cube (q-x) (r+x)  (s  )
        DR -> Cube (q  ) (r+x)  (s-x)

    Cube q1 r1 s1 `hexAdd` Cube q2 r2 s2 = Cube (q1+q2) (r1+r2) (s1+s2)
    Cube q1 r1 s1 `hexSubtract` Cube q2 r2 s2 = Cube (q1-q2) (r1-r2) (s1-s2)
    n `hexTimes` Cube q r s = Cube (n*q) (n*r) (n*s)
    hexZero = Cube 0 0 0

    distance (Cube q1 r1 s1) (Cube q2 r2 s2) = (abs (q1-q2) + abs (r1-r2) + abs (s1-s2)) `div` 2

    rotateCw (Cube q r s) = Cube (-r) (-s) (-q)
    rotateCcw (Cube q r s) = Cube (-s) (-q) (-r)

    toVec2 size = toVec2 size . cubicalToAxial

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

instance HexagonalCoordinate Axial where
    move dir x (Axial q r) = case dir of
        R  -> Axial (q+x) (r  )
        UR -> Axial (q+x) (r-x)
        UL -> Axial (q  ) (r-x)
        L  -> Axial (q-x) (r  )
        DL -> Axial (q-x) (r+x)
        DR -> Axial (q  ) (r+x)

    Axial q1 r1 `hexAdd` Axial q2 r2 = Axial (q1+q2) (r1+r2)
    Axial q1 r1 `hexSubtract` Axial q2 r2 = Axial (q1-q2) (r1-r2)
    n `hexTimes` Axial q r = Axial (n*q) (n*r)
    hexZero = Axial 0 0

    distance a b = distance (axialToCubical a) (axialToCubical b)

    rotateCw = cubicalToAxial . rotateCw . axialToCubical
    rotateCcw = cubicalToAxial . rotateCcw . axialToCubical

    toVec2 size (Axial q r) =
        let q' = fromIntegral q
            r' = fromIntegral r
            x = size * (sqrt(3)*q' + sqrt(3)/2*r')
            y = size * (                   3/2*r')
        in Vec2 x y

    fromVec2 size vec = cubicalToAxial (fromVec2 size vec)

instance Semigroup Axial where
    (<>) = hexAdd

instance Monoid Axial where
    mempty = hexZero

rotateAround :: HexagonalCoordinate hex => hex -> Int -> hex -> hex
rotateAround center n hex =
    let hex' = hex `hexSubtract` center
        rot i x | i > 0 = rot (i-1) (rotateCw x)
                | i < 0 = rot (i+1) (rotateCcw x)
                | otherwise = x
        rotated = rot n hex'
    in rotated `hexAdd` center

-- | 'Polygon' to match a 'HexagonalCoordinate'. Useful e.g. for collision
-- checking, and of course also for painting. :-)
hexagonPoly :: HexagonalCoordinate hex => Double -> hex -> G.Polygon
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
    let hexagons = hexagonsInRange range zero

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
                polygonSketch (G.transform (G.scaleAround zero 0.9) centerHexagon)
                polygonSketch (G.transform (G.scaleAround zero 1.1) centerHexagon)
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
ring :: HexagonalCoordinate a => Int -> a -> [a]
ring n center = do
    (startDir, walkDir) <- zip [R, UR, UL, L, DL, DR] [UL, L, DL, DR, R, UR]
    let start = move startDir n center
    [ move walkDir i start | i <- [0..n-1]]

data Polygon hex = Polygon [hex]
    deriving (Eq, Ord, Show)

isOnEdge :: Cube -> Polygon Cube -> Bool
isOnEdge hex (Polygon corners) =
    let edges = concat (zipWith line corners (cycle corners))
    in isJust (find (== hex) edges)

pointInPolygon :: Cube -> Polygon Cube -> Bool
pointInPolygon hex polygon@(Polygon corners) = onEdge || inside
  where
    -- | This elimintes numerical instabilities
    onEdge = isOnEdge hex polygon

    -- This feels like cheating
    inside = G.pointInPolygon (toVec2 1 hex) (G.Polygon (map (toVec2 1) corners))
