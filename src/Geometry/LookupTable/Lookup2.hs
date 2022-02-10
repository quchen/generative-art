module Geometry.LookupTable.Lookup2 where



import           Data.Foldable
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Vector   (Vector, (!))
import qualified Data.Vector   as V
import           Data.Ord
import           Prelude       hiding (lines)

import Geometry.Core
import Numerics.Interpolation
import Geometry.Trajectory



-- | Discrete 'Vec2'.
data IVec2 = IVec2 !Int !Int
    deriving (Eq, Ord, Show)

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
