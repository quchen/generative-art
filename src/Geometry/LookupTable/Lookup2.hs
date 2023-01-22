-- | Two-dimensional lookup tables.
module Geometry.LookupTable.Lookup2 (
    -- * Function cache
      LookupTable2
    , GridSpec(..)
    , createLookupTable2
    , lookupNearest
    , lookupBilinear
    , forLookupTable2_

    -- * Technical utilities
    , IVec2(..)
    , CIVec2(..)
    , roundCIVec2
    , fromGrid
    , toGrid
    , valueTable
) where



import           Control.DeepSeq
import           Control.Parallel.Strategies
import           Data.Ord.Extended
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as V

import Geometry.Core
import Numerics.Interpolation



-- | Lookup table for a two-dimensional function. Created with 'lookupTable2'.
data LookupTable2 a = LookupTable2 GridSpec (Vector (Vector a))
    deriving (Eq, Ord, Show)

instance NFData a => NFData (LookupTable2 a) where
    rnf (LookupTable2 gridSpec vec) = withStrategy (parTraversable rdeepseq) vec `seq` rnf gridSpec

-- | Build a 2D lookup table, suitable for caching function calls. Values are
-- initialized lazily, so that only repeated computations are sped up.
--
-- === Example: lookup table for \(f(x,y) = x\cdot y\)
--
-- @
-- gridSpec = 'GridSpec' ('Vec2' (-10) (-10), 'Vec2' 10 10), (100, 100)
-- f ('Vec2' x y) = x*y
-- table = 'createLookupTable2' gridSpec f
-- @
createLookupTable2 :: GridSpec -> (Vec2 -> a) -> LookupTable2 a
createLookupTable2 gridSpec f = LookupTable2 gridSpec (valueTable gridSpec f)

-- | Nearest neigbour lookup in a two-dimensional lookup table. Lookup outside of
-- the lookup table’s domain is clamped to the table’s edges.
--
-- Compared to 'lookupBilinear' this function works on types that don’t support
-- arithmetic on them, and is faster. The downside is of course that only the
-- values on the grid points are accessible, without any interpolation.
lookupNearest :: LookupTable2 Double -> Vec2 -> Double
lookupNearest (LookupTable2 gridSpec@(GridSpec _ (iMax, jMax)) vec) xy =
    let CIVec2 iCont jCont = toGrid gridSpec xy
        i = clamp 0 iMax (round iCont)
        j = clamp 0 jMax (round jCont)
    in vec!i!j

-- | Bilinear lookup in a two-dimensional lookup table. Lookup outside of the
-- lookup table’s domain is clamped to the table’s edges, so while it will not make
-- the program crash, the values are not useful.
--
-- This lookup approximates a function in the sense that
--
-- @
-- 'lookupBilinear' ('createLookupTable2' gridSpec f) x ≈ f x
-- @
lookupBilinear :: LookupTable2 Double -> Vec2 -> Double
lookupBilinear (LookupTable2 gridSpec vec) xy =
    let CIVec2 iCont jCont = toGrid gridSpec xy
        iFloor = floor iCont
        jFloor = floor jCont
        iCeil = ceiling iCont
        jCeil = ceiling jCont

        lut_iFloor = vec!iFloor
        lut_iCeil = vec!iCeil

        iFloorValue
            | jFloor /= jCeil = lerp (fromIntegral jFloor, fromIntegral jCeil) (lut_iFloor!jFloor, lut_iFloor!jCeil) jCont
            | otherwise = lut_iFloor!jFloor
        iCeilValue
            | jFloor /= jCeil = lerp (fromIntegral jFloor, fromIntegral jCeil) (lut_iCeil !jFloor, lut_iCeil !jCeil) jCont
            | otherwise = lut_iCeil!jFloor

        result
            | iFloor /= iCeil = lerp (fromIntegral iFloor, fromIntegral iCeil) (iFloorValue, iCeilValue) iCont
            | otherwise = iFloorValue

    in result

-- | Perform an action for each entry in the lookup table. Can be handy for
-- plotting its contents.
--
-- @
-- gridSpec = 'GridSpec' ('Vec2' 0 0, 'Vec2' 100, 100) (100, 100)
-- f ('Vec2' x y) = x + 'sin' y
-- table = 'createLookupTable2' gridSpec f
--
-- 'forLookupTable2_' table $ \val pos _ ->
--     'Draw.moveToVec' pos
--     'Draw.showTextAligned' 'Draw.HCenter' 'Draw.VCenter' ('show' val)
-- @
forLookupTable2_ :: Monad f => LookupTable2 a -> (a -> Vec2 -> IVec2 -> f b) -> f ()
forLookupTable2_ (LookupTable2 gridSpec vec) f =
    V.iforM_ vec $ \i iVec ->
        V.iforM_ iVec $ \j val ->
            let iVec2 = IVec2 i j
                vec2 = fromGrid gridSpec iVec2
            in f val vec2 iVec2

-- | Discrete 'Vec2'. Useful as coordinate in a @'Vector' ('Vector' a)@.
data IVec2 = IVec2 !Int !Int
    deriving (Eq, Ord, Show)

instance NFData IVec2 where rnf _ = ()

-- | Continuous version of 'IVec2'. Type-wise the same as 'Vec2', but it shows
-- fractional grid coodrdinates, so we can express the fact that our lookup might
-- be »between i and i+1« and we can interpolate.
data CIVec2 = CIVec2 !Double !Double
    deriving (Eq, Ord, Show)

instance NFData CIVec2 where rnf _ = ()

-- | Round a »continuous integral« coordinate to a »proper integral« coordinate.
roundCIVec2 :: CIVec2 -> IVec2
roundCIVec2 (CIVec2 i j) = IVec2 (round i) (round j)

-- | Specification of a discrete grid, used for sampling contour lines.
--
-- Subdivide the unit square with 50 squares (51 steps!) in x direction, and 30 (31
-- steps!) in y direction:
--
-- @
-- 'GridSpec' ('Vec2' 0 0, 'Vec2' 1 1) (50, 30)
-- @
data GridSpec = GridSpec
    { _range :: (Vec2, Vec2)  -- ^ Range of continuous coordinates
    , _maxIndex :: (Int, Int) -- ^ Maximum index of the grid, i.e. coordinates range from @(0,0)@ to @'_maxIndex'@.
    } deriving (Eq, Ord, Show)

instance NFData GridSpec where
    rnf (GridSpec (a,b) (c,d)) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

-- | Map a coordinate from the discrete grid to continuous space.
fromGrid
    :: GridSpec
    -> IVec2 -- ^ Discrete coordinate
    -> Vec2  -- ^ Continuous coordinate
fromGrid (GridSpec (Vec2 xMin yMin, Vec2 xMax yMax) (iMax, jMax)) (IVec2 i j) =
    let x = lerp (0, fromIntegral iMax) (xMin, xMax) (fromIntegral i)
        y = lerp (0, fromIntegral jMax) (yMin, yMax) (fromIntegral j)
    in Vec2 x y

toGrid
    :: GridSpec
    -> Vec2 -- ^ Continuous coordinate
    -> CIVec2
            -- ^ Continuous coordinate, scaled and clamped to grid dimensions.
            --   Suitable to be rounded to an 'IVec' with 'roundCIVec2'.
toGrid (GridSpec (Vec2 xMin yMin, Vec2 xMax yMax) (iMax, jMax)) (Vec2 x y) =
    let iContinuous = clamp 0 (fromIntegral iMax) (lerp (xMin, xMax) (0, fromIntegral iMax) x)
        jContinuous = clamp 0 (fromIntegral jMax) (lerp (yMin, yMax) (0, fromIntegral jMax) y)
    in CIVec2 iContinuous jContinuous

-- | A raw value table, filled (lazily) by a function applied to the underlying
-- 'GridSpec'.
--
-- We first index by @i@ and then @j@, so that @vec!i!j@ has the intuitive meaning
-- of »go in @i@/@x@ direction and then in @j@/@y@. The drawback is that this makes
-- the table look like downward columns of @y@ values, indexed by @x@. The more
-- common picture for at least me is to have line numbers and then rows in each
-- line.
valueTable :: GridSpec -> (Vec2 -> a) -> Vector (Vector a)
valueTable gridSpec@GridSpec{_maxIndex = (is, js)} f =
    V.generate (is+1) (\i -> -- »x« direction
        V.generate (js+1) (\j -> -- »y« direction
            f (fromGrid gridSpec (IVec2 i j))))
