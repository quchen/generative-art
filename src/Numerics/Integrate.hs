-- | Compute definite integrals.
module Numerics.Integrate (
      integrateMidpoint
    , integrateSimpson13
) where

import Data.List

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0
{-# INLINE sum' #-}

-- | Numerical integration with the midpoint method. This is one of the simplest
-- integration algorithms.
--
-- https://en.wikipedia.org/wiki/Midpoint_method
integrateMidpoint
    :: Fractional a
    => (a -> a) -- ^ \(f\)
    -> a        -- ^ \(a\)
    -> a        -- ^ \(b\)
    -> Int      -- ^ Number of interval subdivisions
    -> a        -- ^ \(\int_a^b\mathrm dt\, f(t)\)
integrateMidpoint f a b n =
    h * ( f a / 2
        + sum' [f (a + fromIntegral k*h) | k <- [1..n-1]]
        + f b / 2
        )
  where
    h = (b-a)/fromIntegral n

-- | Numerical integration with Simpson’s ⅓ rule, which approximates curves with
-- quadratic polynomials. It’s not that complicated but still does very well on
-- not-so-crazy functions.
--
-- https://en.wikipedia.org/wiki/Simpson%27s_rule
--
-- Useful as a parameter to 'Numerics.ConvergentRecursion.recurseUntilPrecision'.
integrateSimpson13
    :: Fractional a
    => (a -> a) -- ^ \(f\)
    -> a        -- ^ \(a\)
    -> a        -- ^ \(b\)
    -> Int      -- ^ Number of interval subdivisions
    -> a        -- ^ \(\int_a^b\mathrm dt\, f(t)\)
integrateSimpson13 f a b n
    | odd n = integrateSimpson13 f a b (succ n)
integrateSimpson13 f a b n =
    h/3 * ( f a
          + 2 * sum' [f (x (2*j  )) | j <- [1..n`div`2-1]]
          + 4 * sum' [f (x (2*j-1)) | j <- [1..n`div`2]]
          + f b
          )
  where
    x i = a + (fromIntegral (i :: Int))*h
    h = (b-a)/fromIntegral n
