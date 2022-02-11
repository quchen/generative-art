module Geometry.LookupTable.Lookup2 where



import           Data.Vector   (Vector, (!))
import qualified Data.Vector   as V
import           Data.Ord
import           Prelude       hiding (lines)

import Geometry.Core
import Numerics.Interpolation



-- | Discrete 'Vec2'. Useful as coordinate in a @'Vector' ('Vector' a)@.
data IVec2 = IVec2 !Int !Int
    deriving (Eq, Ord, Show)

-- | Continuous version of 'IVec2'. Type-wise the same as 'Vec2', but it shows
-- fractional grid coodrdinates, so we can express the fact that our lookup might
-- be »between i and i+1« and we can interpolate.
data CIVec2 = CIVec2 !Double !Double
    deriving (Eq, Ord, Show)

roundCIVec2 :: CIVec2 -> IVec2
roundCIVec2 (CIVec2 i j) = IVec2 (round i) (round j)

-- | Specification of a discrete grid, used for sampling contour lines.
--
-- Subdivide the unit square with 50 steps in x direction, and 30 in y direction:
--
-- @
-- 'Grid' ('Vec2' 0 0, 'Vec2' 1 1) (50, 30)
-- @
data Grid = Grid
    { _range :: (Vec2, Vec2)  -- ^ Range of continuous coordinates
    , _numCells :: (Int, Int) -- ^ Number of grid coordinates, i.e. the grid's resolution.
    } deriving (Eq, Ord, Show)

-- | Map a coordinate from the discrete grid to continuous space.
fromGrid
    :: Grid
    -> IVec2 -- ^ Discrete coordinate
    -> Vec2  -- ^ Continuous coordinate
fromGrid (Grid (Vec2 xMin yMin, Vec2 xMax yMax) (iMax, jMax)) (IVec2 i j) =
    let x = linearInterpolate (0, fromIntegral iMax) (xMin, xMax) (fromIntegral i)
        y = linearInterpolate (0, fromIntegral jMax) (yMin, yMax) (fromIntegral j)
    in Vec2 x y

toGrid
    :: Grid
    -> Vec2 -- ^ Continuous coordinate
    -> CIVec2
            -- ^ Continuous coordinate, scaled to grid dimensions.
            --   Suitable to be rounded to an 'IVec' and then used
            --   for a lookup table query.
toGrid (Grid (Vec2 xMin yMin, Vec2 xMax yMax) (iMax, jMax)) (Vec2 x y) =
    let iContinuous = linearInterpolate (xMin, xMax) (0, fromIntegral iMax) x
        jContinuous = linearInterpolate (yMin, yMax) (0, fromIntegral jMax) y
    in CIVec2 iContinuous jContinuous

-- | We first index by i and then j, so that vec!i!j has the intuitive meaning of »go in i/x direction and then in j/y.
-- The drawback is that this makes the table look like downward columns of y
-- values, indexed by x. The more common picture for at least me is to have line
-- numbers and then rows in each line.
valueTable :: Grid -> (Vec2 -> a) -> Vector (Vector a)
valueTable grid@Grid{_numCells = (is, js)} f =
    -- We add 1 here so that we include the bottom-right, bottom-left,
    -- and top-right corners of the cells at the bottom and right
    -- Grid edges as well. Without the +1, we only get the value at the
    -- top left of each cell, which is only sufficient for cells inside
    -- the grid (because their bottom-right will be covered as another cell’s
    -- top-left).
    V.generate (is+1) (\i -> -- »x« direction
        V.generate (js+1) (\j -> -- »y« direction
            f (fromGrid grid (IVec2 i j))))

-- | Lookup table for a two-dimensional function.
data LookupTable2 a = LookupTable2 Grid (Vector (Vector a))
    deriving (Eq, Ord, Show)

-- | Build a 2D lookup table, suitable for caching function calls. Values are
-- initialized lazily, so that only repeated computations are sped up.
lookupTable2 :: Grid -> (Vec2 -> a) -> LookupTable2 a
lookupTable2 grid f = LookupTable2 grid (valueTable grid f)

-- | Bilinear lookup in a two-dimensional lookup table. Lookup outside of the
-- lookup table’s domain is clamped to the table’s edges, so while it will not make
-- the program crash, the values are not useful.
lookupBilinear :: LookupTable2 Double -> Vec2 -> Double
lookupBilinear (LookupTable2 grid@(Grid _ (iMax, jMax)) vec) xy =
    let CIVec2 iCont jCont = toGrid grid xy
        iFloor = max 0 (floor iCont)
        jFloor = max 0 (floor jCont)
        iCeil = min iMax (ceiling iCont)
        jCeil = min jMax (ceiling jCont)

        lut_iFloor = vec!iFloor
        lut_iCeil = vec!iCeil

        iFloorValue
            | jFloor /= jCeil = linearInterpolate (fromIntegral jFloor, fromIntegral jCeil) (lut_iFloor!jFloor, lut_iFloor!jCeil) jCont
            | otherwise = lut_iFloor!jFloor
        iCeilValue
            | jFloor /= jCeil = linearInterpolate (fromIntegral jFloor, fromIntegral jCeil) (lut_iCeil !jFloor, lut_iCeil !jCeil) jCont
            | otherwise = lut_iCeil!jFloor

        result
            | iFloorValue /= iCeilValue = linearInterpolate (fromIntegral iFloor, fromIntegral iCeil) (iFloorValue, iCeilValue) iCont
            | otherwise = iFloorValue

    in result
