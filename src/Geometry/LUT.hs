module Geometry.LUT (
      VLUT(..)
    , S(..)
    , T(..)
    , lookupInterpolated
    , lookupBiasLower
) where

import qualified Data.Vector   as V
import           Geometry.Core

-- | Vector-based lookup table from 'fst' to 'snd'. Values must increase with vector index, enabling binary search.
newtype VLUT a b = VLUT (V.Vector (a, b))
    deriving (Eq, Ord, Show)

-- | Safety newtype wrapper because it’s super confusing that Runge-Kutta commonly
-- has t for time, but here we have s(t) where s corredponds to time in RK.
--
-- 'T' is used for simple Bezier parametrization, 'S' for curve-length based.
newtype T a = T a deriving (Eq, Ord, Show)

instance Num a => Num (T a) where
    T a + T b = T (a+b)
    T a - T b = T (a-b)
    T a * T b = T (a*b)
    abs (T a) = T (abs a)
    signum (T a) = T (signum a)
    negate (T a) = T (negate a)
    fromInteger i = T (fromInteger i)

instance Fractional a => Fractional (T a) where
    T a / T b = T (a/b)
    recip (T a) = T (recip a)
    fromRational r = T (fromRational r)

instance VectorSpace a => VectorSpace (T a) where
    T a +. T b = T (a+.b)
    a *. T b = T (a*.b)
    T a -. T b = T (a-.b)

-- | See 'T'.
--
-- 'T' is used for simple Bezier parametrization, 'S' for curve-length based.
newtype S a = S a deriving (Eq, Ord, Show)

instance Num a => Num (S a) where
    S a + S b = S (a+b)
    S a - S b = S (a-b)
    S a * S b = S (a*b)
    abs (S a) = S (abs a)
    signum (S a) = S (signum a)
    negate (S a) = S (negate a)
    fromInteger i = S (fromInteger i)

-- | Look up the index of a value, reducing the search space by binary search until
-- it has size 1.
--
-- If the value is not actually in the LUT but between the LUT’s values, you can
-- use 'interpolate' to get a linear interpolation.
lookupIndex :: Ord a => VLUT a b -> a -> Int
lookupIndex (VLUT lut) needle = search 0 (V.length lut)
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

-- | Find t(s) from a LUT using binary search, interpolating linearly around the
-- search result. Clips for out-of-range values.
lookupInterpolated :: VLUT (S Double) (T Double) -> S Double -> T Double
lookupInterpolated lut needle = interpolate lut needle (lookupIndex lut needle)

-- | Lookup in the LUT, with bias towards the left, i.e. when searching a value not
-- present in the LUT, return the closest one before.
lookupBiasLower :: Ord a => VLUT a b -> a -> (a, b)
lookupBiasLower lut@(VLUT rawLut) needle
  = let ix = lookupIndex lut needle
    in rawLut V.! ix

-- | Look at left/right neighbours. If they’re there and the needle is between it
-- and the found pivot index, then linearly interpolate the result value between
-- them.
interpolate :: VLUT (S Double) (T Double) -> S Double -> Int -> T Double
interpolate (VLUT lut) needle pivotIndex = case (lut V.!? (pivotIndex-1), lut V.! pivotIndex, lut V.!? (pivotIndex+1)) of
    -- Interpolate between pivot and left neighbour?
    (Just (leftS, leftT), (pivotS, pivotT), _)
        | between (leftS, pivotS) needle -> linearInterpolate (leftS, pivotS) (leftT, pivotT) needle
    -- Interpolate between pivot and right neighbour?
    (_, (pivotS, pivotT), Just (rightS, rightT))
        | between (pivotS, rightS) needle -> linearInterpolate (pivotS, rightS) (pivotT, rightT) needle
    -- Fallback: don’t interpolate
    (_, (_, pivotT), _) -> pivotT

-- Linearly interpolate the interval [a,b] to [x,y] and get the point t
linearInterpolate :: (S Double, S Double) -> (T Double, T Double) -> S Double -> T Double
linearInterpolate (S a, S b) (T x, T y) (S s)
    = T (x + (y-x)/(b-a) * (s-a))

between :: Ord a => (a, a) -> a -> Bool
between (a,b) x = min a b < x && x < max a b
