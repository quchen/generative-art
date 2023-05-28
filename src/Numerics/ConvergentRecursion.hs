-- | Apply algorithms repeatedly until we’re happy with the result.
module Numerics.ConvergentRecursion (
      retryLinearlyUntilPrecision
    , retryExponentiallyUntilPrecision
    , recurseUntilPrecision
) where

import qualified Data.List as L

-- | Search an infinite list for two consecutive values whose relative distance is
-- smaller than the precision parameter.
findCloseConsecutives :: [Double] -> Double -> Double
findCloseConsecutives values precision
  = let closeEnoughPair (x,y) = (x-y)/x < precision
        Just (_good, evenBetter) = L.find closeEnoughPair (pairsOf values)
    in evenBetter

pairsOf :: [a] -> [(a, a)]
pairsOf xs = zip xs (tail xs)

-- | Retry applying a function until two consecutive results are close enough
-- together.
--
-- Each attempt uses twice as many iterations as the last one (hence
-- exponentially).
retryExponentiallyUntilPrecision
    :: (Int -> Double) -- ^ Function of a number of iterations to perform, e.g. integration subdivisions
    -> Double          -- ^ Precision: relative error threshold between two iterations to accept the result
    -> Double          -- ^ Result
retryExponentiallyUntilPrecision f precision = findCloseConsecutives [f (2^n) | n <- [1..]] precision

-- | Retry applying a function until two consecutive results are close enough
-- together.
--
-- Each attempt uses one more step than the last one (hence linearly).
retryLinearlyUntilPrecision
    :: (Int -> Double) -- ^ Function of a number of iterations to perform, e.g. integration subdivisions
    -> Double          -- ^ Precision: relative error threshold between two iterations to accept the result
    -> Double          -- ^ Result
retryLinearlyUntilPrecision f precision = findCloseConsecutives [f n | n <- [0..]] precision

-- | Recursively apply a function to a value, until the relative distance between
-- two iterations is below the precision parameter.
--
-- Find the a root of \(x^2-1=0\):
--
--
-- >>> let f x = x^2-1
-- >>> recurseUntilPrecision (Numerics.FindRoot.newtonStep 1e-3 f ) 2 1e-10
-- 1.0
recurseUntilPrecision
    :: (Double -> Double) -- ^ Function to iterate
    -> Double             -- ^ Initial value
    -> Double             -- ^ Desired precision
    -> Double
recurseUntilPrecision f x0 = findCloseConsecutives (iterate f x0)
