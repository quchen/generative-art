-- | Solve @f(x) = 0@ for @x@.
module Numerics.FindRoot where

-- | Single step for Newton’s method for root finding.
--
-- https://en.wikipedia.org/wiki/Newton%27s_method
--
-- Useful as a parameter to 'Numerics.ConvergentRecursion.recurseUntilPrecision'.
newtonStep
    :: (Double -> Double) -- ^ f(t_n)
    -> (Double -> Double) -- ^ f'(t_n)
    -> Double             -- ^ t_n
    -> Double             -- ^ t_{n+1}
newtonStep f f' t = t - f t/f' t
