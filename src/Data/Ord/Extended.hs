module Data.Ord.Extended (
      clamp
    , between
    , module Data.Ord
) where

import Data.Ord

-- | Constrain a number between two values: find the closest value within an
-- interval specified by its boundary points.
--
-- >>> clamp 0 1 0.5
-- 0.5
--
-- >>> clamp 0 1 999
-- 1
-- >>> clamp 1 0 999
-- 1
-- >>> clamp 1 0 (-999)
-- 0
clamp
    :: Ord a
    => a -- ^ First interval boundary
    -> a -- ^ Second interval boundary
    -> a -- ^ Input value
    -> a -- ^ Closest value in the interval
clamp a b x =
    let lo = min a b
        hi = max a b
    in min hi (max x lo)

-- | Is the value between two other values?
between :: Ord a => (a, a) -> a -> Bool
between (a,b) x = min a b < x && x < max a b
