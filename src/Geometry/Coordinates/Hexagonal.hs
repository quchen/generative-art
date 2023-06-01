-- | Hexagonal coordinate systems.
--
-- Nice article about the topic: https://www.redblobgames.com/grids/hexagons/
--
-- <<docs/haddock/Geometry/Coordinates/Hexagonal/cubes.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Coordinates/Hexagonal/cubes.svg" 360 360 $ do
--     let cellSize = 10
--         hexagons = runST $ do
--             gen <- MWC.create
--             points <- gaussianDistributedPoints gen canvas (40*.mempty) (2^14)
--             let hexs = fmap (fromVec2 cellSize) points
--             pure $ foldl' (\weight hex -> M.insertWith (+) hex (1::Int) weight) mempty hexs
--         canvas = shrinkBoundingBox (cellSize*2) [zero, Vec2 360 360]
--         MinMax minW maxW = foldMap (\x -> MinMax x x) hexagons
--     for_ (sortOn (\(_,weight) -> weight) (M.toList hexagons)) $ \(hex, weight) -> do
--         let value = lerp ((sqrt (fromIntegral minW)), (sqrt (fromIntegral maxW))) (0,1) (sqrt (fromIntegral weight))
--             growth = lerp (0,1) (3,6) value
--         sketch (growPolygon growth (hexagonPoly cellSize hex))
--         let color = twilight value
--         setColor color
--         C.fillPreserve
--         let hexCenter = toVec2 cellSize hex
--         sketch [Line hexCenter (hexCenter +. polar (deg (d+30)) (cellSize + 2/sqrt 3*growth)) | d <- [0,120,240]]
--         setColor black
--         C.stroke
-- :}
-- Generated file: size 194KB, crc32: 0xcde33b0b
module Geometry.Coordinates.Hexagonal (
      Hex(..)
    , toVec2
    , fromVec2

    -- * Painting aid
    , hexagonalCoordinateSystem

    -- * Movement
    , Direction(..)
    , move

    -- * Arithmetic
    , hexAdd
    , hexSubtract
    , hexTimes
    , hexZero

    -- * Measurement and transformation
    , distance
    , rotateAround
    , cubeRound

    -- * Geometry
    , line
    , ring
    , hexagonsInRange
    , HexPolygon(..)
    , isOnEdge
    , pointInPolygon
    , edgePoints
    , floodFill

    -- * Drawing and interfacing
    , polygonSketch
    , hexagonPoly
) where



import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Graphics.Rendering.Cairo as C hiding (x, y)

import           Draw
import           Geometry.Core          as G hiding
    (Polygon, pointInPolygon, rotateAround)
import qualified Geometry.Core          as G
import           Numerics.Interpolation
import           Util



-- $setup
-- >>> import           Control.Monad.ST
-- >>> import           Data.Foldable
-- >>> import qualified Data.Map                        as M
-- >>> import           Data.Ord.Extended
-- >>> import           Draw
-- >>> import           Data.List
-- >>> import           Geometry.Algorithms.Sampling
-- >>> import qualified System.Random.MWC               as MWC
-- >>> import qualified System.Random.MWC.Distributions as MWC



-- | Hexagonal coordinate.
data Hex = Hex !Int !Int
    -- ^ The choice of values is called »cubal«.
    -- Use 's' (= -q-r) to get the omitted coordinate’s value.

    -- This is really just a ℝ^3 with rounding occurring in every calculation,
    -- but alas, ℤ is not a field, so it isn’t a vector space.
    deriving (Show)

instance Eq Hex where
    Hex q r == Hex q' r' = q == q' && r == r'

instance Ord Hex where
    Hex q r `compare` Hex q' r' = compare q q' <> compare r r'

instance NFData Hex where
    rnf _ = () -- Constructors are already strict

-- | Since \(q+r+s=0\) in cubical hexagonal coordinates, we can infer the third
-- from the other two.
s :: Int -> Int -> Int
s q r = -q-r

-- | Hexagonal direction, used by 'move'.
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
move dir x (Hex q r) = case dir of
    R  -> Hex (q+x)  r
    UR -> Hex (q+x) (r-x)
    UL -> Hex  q    (r-x)
    L  -> Hex (q-x)  r
    DL -> Hex (q-x) (r+x)
    DR -> Hex  q    (r+x)

-- | Add two 'Hex' coordinates.
hexAdd :: Hex -> Hex -> Hex
Hex q1 r1 `hexAdd` Hex q2 r2 = Hex (q1+q2) (r1+r2)

-- | Subtract two 'Hex' coordinates.
hexSubtract :: Hex -> Hex -> Hex
Hex q1 r1 `hexSubtract` Hex q2 r2 = Hex (q1-q2) (r1-r2)

-- | Multiply a 'Hex' coordinate with a whole number.
hexTimes :: Int -> Hex -> Hex
n `hexTimes` Hex q r = Hex (n*q) (n*r)

-- | The origin of the hexagonal coordinate system.
hexZero :: Hex
hexZero = Hex 0 0

-- | How many steps are between two coordinates?
distance :: Hex -> Hex -> Int
distance (Hex q1 r1) (Hex q2 r2) = (abs (q1-q2) + abs (r1-r2) + abs (s1-s2)) `div` 2
  where
    s1 = s q1 r1
    s2 = s q2 r2

-- | Rotate clockwise by 60°
rotateCw :: Hex -> Hex
rotateCw (Hex q r) = Hex (-r) (-s q r)

-- | Rotate counterclockwise by 60°
rotateCcw :: Hex -> Hex
rotateCcw (Hex q r) = Hex (-s q r) (-q)

-- | Mirror on the origin.
mirror0 :: Hex -> Hex
mirror0 (Hex q r) = Hex (-q) (-r)

-- | Rotate around a center by a number of 60° angles.
rotateAround
    :: Hex -- ^ Center
    -> Int -- ^ number of 60° rotations. Positive for clockwise (in Cairo coordinates).
    -> Hex -- ^ Point to rotate
    -> Hex
rotateAround center n =
    (`hexAdd` center) . rotateAround0 n . (`hexSubtract` center)

-- | Rotate around the origin
rotateAround0 :: Int -> Hex -> Hex
rotateAround0 n = go (mod n 6)
  where
    go 0 = id
    go 1 = rotateCw
    go 2 = rotateCw . rotateCw
    go 3 = mirror0
    go 4 = rotateCcw . rotateCcw
    go 5 = rotateCcw
    go i = bugError "Hexagonal.rotateAround0" ("Bad modulus in rotateAround, got value: " ++ show i)

-- | Convert a hexagonal coordinate’s center to an Euclidean 'Vec2'.
toVec2
    :: Double -- ^ Size of a hex cell (radius, side length)
    -> Hex
    -> Vec2
toVec2 size (Hex q r) =
    let q' = fromIntegral q
        r' = fromIntegral r
        x = size * (sqrt 3*q' + sqrt 3/2*r')
        y = size * (                 3/2*r')
    in Vec2 x y

-- | Convert a Euclidean 'Vec2' to the coordiante of the hexagon it is in.
fromVec2
    :: Double -- ^ Size of a hex cell (radius, side length)
    -> Vec2
    -> Hex
fromVec2 size (Vec2 x y) =
    let q', r' :: Double
        q' = (sqrt 3/3 * x - 1/3 * y) / size
        r' = (               2/3 * y) / size
    in cubeRound q' r'

-- | 'hexAdd'
instance Semigroup Hex where
    (<>) = hexAdd

-- | 'hexZero'
instance Monoid Hex where
    mempty = hexZero

-- | Given fractional cubical coordinates, yield the hexagon the coordinate is in.
cubeRound :: Double -> Double -> Hex
cubeRound q' r'  =
    let s' = -q'-r'
        qq,rr,ss :: Int
        qq = round q'
        rr = round r'
        ss = round s'

        -- Rounding all three might violate the invariant that
        -- q+r+s=0, so we calculate the discrepancy and discard
        -- the value that was rounded the most.
        qDiff, rDiff, sDiff :: Double
        qDiff = abs (fromIntegral qq - q')
        rDiff = abs (fromIntegral rr - r')
        sDiff = abs (fromIntegral ss - s')
    in if
        -- q had highest diff
        | qDiff > rDiff && qDiff > sDiff -> Hex (-rr-ss) rr
        -- r had highest diff
        | rDiff > sDiff                  -> Hex qq (-qq-ss)
        -- s is the only one left
        | otherwise                      -> Hex qq rr

-- | 'Polygon' to match a 'HexagonalCoordinate'. Useful e.g. for collision
-- checking, and of course also for painting. :-)
hexagonPoly :: Double -> Hex -> G.Polygon
hexagonPoly sideLength hex =
    let center = toVec2 sideLength hex
    in G.transform (G.translate center <> G.scale sideLength) unitHexagon

-- | Hexagon of side length 1, centered around (0,0).
unitHexagon :: G.Polygon
unitHexagon = G.Polygon [polar (deg (30+60*n)) 1 | n <- [0..5]]

-- | Draw a hexagonal coordinate system as a helper grid, similar to
-- 'Draw.cartesianCoordinateSystem'.
hexagonalCoordinateSystem
    :: Double -- ^ Side length of a hexagon (equivalent to its radius)
    -> Int    -- ^ How many hexagons to draw in each direction
    -> C.Render ()
hexagonalCoordinateSystem sideLength range = do
    let hexagons = hexagonsInRange range hexZero

    cairoScope $ grouped (C.paintWithAlpha 0.2) $ do
        -- Variable names use Cairo coordinates, i.e. inverted y axis compared to math.

        -- First, we draw the happy path: the left-hand side of all hexagons.
        -- This will leave the ones at the top, bottom and right partially open,
        -- which we fix later.
        C.setLineWidth 1
        C.setSourceRGB 0 0 0
        for_ hexagons $ \hexCoord@(Hex q r) -> do
            let center = toVec2 sideLength hexCoord
                bottomCorner = center +. Vec2 0 sideLength
                rotateCW degrees = G.rotateAround center (deg degrees)
                corner i = G.transform (rotateCW (i*60)) bottomCorner
            if
                -- Rightmost corner: the only full hexagon, woo!
                | q == range && s q r == -range -> sketch $ G.Polygon [corner i | i <- [0, 1, 2, 3, 4, 5]]
                -- Upper right boundary
                | q == range                -> sketch $ Polyline [corner i | i <- [0, 1, 2, 3, 4, 5]]
                -- Lower right boundary
                | s q r == -range           -> sketch $ Polyline [corner i | i <- [-2, -1, 0, 1, 2, 3]]
                -- Upper boundary
                | r == -range               -> sketch $ Polyline [corner i | i <- [0, 1, 2, 3, 4]]
                -- Lower boundary
                | r == range                -> sketch $ Polyline [corner i | i <- [-1, 0, 1, 2, 3]]
                | otherwise                 -> sketch $ Polyline [corner i | i <- [0, 1, 2, 3]]
            when (hexCoord == hexZero) $ do
                let centerHexagon = G.Polygon [corner i | i <- [0, 1, 2, 3, 4, 5]]
                sketch (G.transform (G.scaleAround zero 0.9) centerHexagon)
                sketch (G.transform (G.scaleAround zero 1.1) centerHexagon)
            C.stroke

    cairoScope $ grouped (C.paintWithAlpha 0.5) $
        for_ hexagons $ \hexCoord@(Hex q r) ->
            for_ [("q" :: String, q, 120), ("r", r, 240), ("s", s q r, 0)] $ \(name, val, angle) -> cairoScope $ do
                let center = toVec2 sideLength hexCoord
                    coord = G.transform (scaleAround center 0.2 <> G.rotateAround center (deg angle)) (center +. Vec2 0 sideLength)
                moveToVec coord
                setColor (hsva angle 1 0.7 1)
                if Hex q r == hexZero
                    then cairoScope (C.setFontSize 14 >> showTextAligned HCenter VCenter name)
                    else showTextAligned HCenter VCenter (show val)

-- | Hexagons reachable within a number of steps from the origin. The boundary of
-- this will be the 'ring'.
hexagonsInRange :: Int -> Hex -> [Hex]
hexagonsInRange range center = do
    q <- [-range,-range+1..range]
    let rMin = max (-range) (-q-range)
        rMax = min range (-q+range)
    r <- [rMin, rMin+1 .. rMax]
    pure (Hex q r `hexAdd` center)

-- | Linearly interpolate between two 'Hex'.
cubeLerp
    :: Hex    -- ^ Start
    -> Hex    -- ^ End
    -> Double -- [0..1] yields [start..end]
    -> Hex
cubeLerp (Hex q1 r1) (Hex q2 r2) t =
    cubeRound
        (lerp (0,1) (fromIntegral q1, fromIntegral q2) t)
        (lerp (0,1) (fromIntegral r1, fromIntegral r2) t)

-- | Line between two 'Hex'.
--
-- <<docs/haddock/Geometry/Coordinates/Hexagonal/line.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Coordinates/Hexagonal/line.svg" 300 200 $ do
--     let cellSize = 20
--         canvas = shrinkBoundingBox 10 [zero, Vec2 300 200]
--         hexes = line hexZero (move R 5 (move UR 3 hexZero))
--         polygons = map (shrinkPolygon 1 . hexagonPoly cellSize) hexes
--         fitToCanvas = transform (transformBoundingBox polygons canvas def)
--     for_ polygons $ \polygon -> cairoScope $ do
--         sketch (fitToCanvas polygon)
--         setColor (mathematica97 0 `withOpacity` 0.3)
--         C.fillPreserve
--         setColor (mathematica97 0 `withOpacity` 0.5)
--         C.stroke
-- :}
-- Generated file: size 6KB, crc32: 0x5ec289a8
line :: Hex -> Hex -> [Hex]
line start end =
    let d = distance start end
    in [ cubeLerp start end (1/fromIntegral d*fromIntegral i) | i <- [0..d] ]

-- | All 'Hex' reachable only with one exact number of steps. 'floodFill'ing it
-- will yield 'hexagonsInRange'.
--
-- <<docs/haddock/Geometry/Coordinates/Hexagonal/ring.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Coordinates/Hexagonal/ring.svg" 200 200 $ do
--     let cellSize = 20
--         canvas = shrinkBoundingBox 10 [zero, Vec2 200 200]
--         hexes = ring 2 hexZero
--         polygons = map (shrinkPolygon 1 . hexagonPoly cellSize) hexes
--         fitToCanvas = transform (transformBoundingBox polygons canvas def)
--     for_ polygons $ \polygon -> cairoScope $ do
--         sketch (fitToCanvas polygon)
--         setColor (mathematica97 1 `withOpacity` 0.3)
--         C.fillPreserve
--         setColor (mathematica97 1 `withOpacity` 0.5)
--         C.stroke
-- :}
-- Generated file: size 7KB, crc32: 0xacce7f95
ring
    :: Int -- ^ Radius
    -> Hex -- ^ Center
    -> [Hex]
ring n center = do
    (startDir, walkDir) <- zip [R, UR, UL, L, DL, DR] [UL, L, DL, DR, R, UR]
    let start = move startDir n center
    [ move walkDir i start | i <- [0..n-1]]

newtype HexPolygon = HexPolygon [Hex]
    deriving (Eq, Ord, Show)

-- | Given a hexagonal polygon, is the 'Hex' on its edge?
isOnEdge :: Hex -> HexPolygon -> Bool
isOnEdge hex (HexPolygon corners) =
    let edges = concat (zipWith line corners (tail (cycle corners)))
    in isJust (find (== hex) edges)

-- | Is the 'Hex' inside the polygon (including its edge)?
pointInPolygon :: Hex -> HexPolygon -> Bool
pointInPolygon hex polygon@(HexPolygon corners) = onEdge || inside
  where
    -- | This elimintes numerical instabilities
    onEdge = isOnEdge hex polygon

    -- This feels like cheating
    inside = G.pointInPolygon (toVec2 1 hex) (G.Polygon (map (toVec2 1) corners))

-- | All points on a polygon’s edge.
edgePoints :: HexPolygon -> S.Set Hex
edgePoints (HexPolygon corners) = S.fromList (concat (zipWith line corners (tail (cycle corners))))

-- | Sketch a hexagonal polygon.
polygonSketch
    :: Double -- ^ Cell size
    -> HexPolygon
    -> C.Render ()
polygonSketch cellSize polygon =
    for_ (edgePoints polygon) $ \hex ->
        sketch (hexagonPoly cellSize hex)

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
