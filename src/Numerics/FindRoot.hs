-- | Solve \(f(x) = 0\) for \(x\).
module Numerics.FindRoot (newtonStep) where

-- | Single step for Newton’s method for root finding.
--
-- https://en.wikipedia.org/wiki/Newton%27s_method
--
-- \[
-- t_{n+1} = t_n - \frac{f(t_n)}{f'(t_n)}
-- \]
--
-- Useful as a parameter to 'Numerics.ConvergentRecursion.recurseUntilPrecision'.
newtonStep
    :: Double             -- ^ \(h\) to calculate the derivative, as in \(\frac{f(x+h)-f(x)}h\)
    -> (Double -> Double) -- ^ \( f(t_n)  \)
    -> Double             -- ^ \( t_n     \)
    -> Double             -- ^ \( t_{n+1} \)
newtonStep h f t = t - f t / d h f t

-- | Derivative.
d :: Double               -- ^ \(h\) as in \(\frac{f(x+h)-f(x)}h\)
    -> (Double -> Double) -- ^ \(f\)
    -> Double             -- ^ \(x\)
    -> Double             -- ^ \(\partial_xf(x)\)
d h f t = (f (t+h) - f t) / h
