-- | One-dimensional lookup tables
module Geometry.LookupTable.Lookup1 (
      LookupTable1(..)
    , lookupInterpolated
    , lookupBiasLower
) where



import           Control.DeepSeq
import           Data.Ord.Extended
import qualified Data.Vector       as V

import Numerics.Interpolation



-- | Vector-based lookup table. Functions assume that values must increase with
-- vector index, enabling binary search.
newtype LookupTable1 a b = LookupTable1 (V.Vector (a, b))
    deriving (Eq, Ord, Show)

instance (NFData a, NFData b) => NFData (LookupTable1 a b) where
    rnf (LookupTable1 vec) = rnf vec

-- | Look up the index of a value, reducing the search space by binary search until
-- it has size 1.
--
-- If the value is not actually in the LUT but between the LUT’s values, you can
-- use 'interpolate' to get a linear interpolation.
lookupIndex :: Ord a => LookupTable1 a b -> a -> Int
lookupIndex (LookupTable1 lut) needle = search 0 (V.length lut)
  where
    search lo hi
        | lo >= mid = mid
        | hi <= mid = mid
        | otherwise
            = let (pivotS, _) = lut V.! mid
            in case compare pivotS needle of
                LT -> search mid hi
                EQ -> mid
                GT -> search lo mid
        where
        mid = (hi+lo) `div` 2

-- | Find a value in the lookup table using binary search, interpolating linearly
-- around the search result. Clips for out-of-range values.
lookupInterpolated :: LookupTable1 Double Double -> Double -> Double
lookupInterpolated lut needle = interpolate lut needle (lookupIndex lut needle)

-- | Lookup in the LUT, with bias towards the left, i.e. when searching a value not
-- present in the LUT, return the closest one before.
lookupBiasLower :: Ord a => LookupTable1 a b -> a -> (a, b)
lookupBiasLower lut@(LookupTable1 rawLut) needle
  = let ix = lookupIndex lut needle
    in rawLut V.! ix

-- | Look at left/right neighbours. If they’re there and the needle is between it
-- and the found pivot index, then linearly interpolate the result value between
-- them.
interpolate :: LookupTable1 Double Double -> Double -> Int -> Double
interpolate (LookupTable1 lut) needle pivotIndex = case (lut V.!? (pivotIndex-1), lut V.! pivotIndex, lut V.!? (pivotIndex+1)) of
    -- Interpolate between pivot and left neighbour?
    (Just (leftS, leftT), (pivotS, pivotT), _)
        | between (leftS, pivotS) needle -> lerp (leftS, pivotS) (leftT, pivotT) needle
    -- Interpolate between pivot and right neighbour?
    (_, (pivotS, pivotT), Just (rightS, rightT))
        | between (pivotS, rightS) needle -> lerp (pivotS, rightS) (pivotT, rightT) needle
    -- Fallback: don’t interpolate
    (_, (_, pivotT), _) -> pivotT
