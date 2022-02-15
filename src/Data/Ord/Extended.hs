{-# LANGUAGE FlexibleInstances #-}

module Data.Ord.Extended (
      clamp
    , between
    , Min(..)
    , Max(..)
    , MinMax(..)
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

infinity :: Double
infinity = 1/0

-- | 'Monoid' to calculate the minimum.
newtype Min a = Min a deriving (Eq, Ord, Show)
instance Ord a => Semigroup (Min a) where Min min1 <> Min min2 = Min (min min1 min2)
-- | 'mempty' = \(\infty\)
instance {-# OVERLAPPING #-} Monoid (Min Double) where mempty = Min infinity
instance {-# OVERLAPPABLE #-} (Bounded a, Ord a) => Monoid (Min a) where mempty = Min maxBound

-- | 'Monoid' to calculate the maximum.
newtype Max a = Max a deriving (Eq, Ord, Show)
instance Ord a => Semigroup (Max a) where Max max1 <> Max max2 = Max (max max1 max2)
-- | 'mempty' = \(-\infty\)
instance {-# OVERLAPPING #-} Monoid (Max Double) where mempty = Max (-infinity)
instance {-# OVERLAPPABLE #-} (Bounded a, Ord a) => Monoid (Max a) where mempty = Max minBound

-- | 'Semigroup' to calculate the minimum and maximum simultaneously.
data MinMax a = MinMax !a !a deriving (Eq, Ord, Show)
instance Ord a => Semigroup (MinMax a) where MinMax min1 max1 <> MinMax min2 max2 = MinMax (min min1 min2) (max max1 max2)
-- | 'mempty' = \((\infty, -\infty)\)
instance {-# OVERLAPPING #-} Monoid (MinMax Double) where mempty = MinMax infinity (-infinity)
instance {-# OVERLAPPABLE #-} (Bounded a, Ord a) => Monoid (MinMax a) where mempty = MinMax maxBound minBound
