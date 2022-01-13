-- | Interpolate between values.
module Numerics.Interpolation (
    linearInterpolate
) where

-- | Linearly interpolate the interval [a,b] to [x,y].
--
-- >>> linearInterpolate (0,1) (10,20) 0.5
-- 15
linearInterpolate :: (Double, Double) -> (Double, Double) -> Double -> Double
linearInterpolate (a, b) (x, y) t = x + (y-x)/(b-a) * (t-a)
