-- | Interpolate between values.
module Numerics.Interpolation (
      linearInterpolate

    , linearInterpolateDD
    , linearInterpolateDI
    , linearInterpolateID
    , linearInterpolateII
) where

-- | Linearly interpolate the interval \([a,b]\) to \([x,y]\). In computer graphics
-- lingo, this is often known as /lerp/.
--
-- \[
-- f(t) = x + \frac{y-x}{b-a} (t-a)
-- \]
--
-- >>> linearInterpolate (0,1) (10,20) 0.5
-- 15
linearInterpolate :: (Double, Double) -> (Double, Double) -> Double -> Double
linearInterpolate (a, b) (x, y) t = x + (y-x)/(b-a) * (t-a)

-- | Linear interpolation from 'Double' to 'Double' (hence: @DD@). This is just a
-- nomenclature-matching synonym for 'linearInterpolate'.
linearInterpolateDD :: (Double, Double) -> (Double, Double) -> Double -> Double
linearInterpolateDD = linearInterpolate

-- | Linear interpolation from 'Double' to 'Integral' (hence: @DI@).
linearInterpolateDI :: Integral int => (Double, Double) -> (int, int) -> Double -> int
linearInterpolateDI ab xy = round . linearInterpolate ab (both fromIntegral xy)

-- | Linear interpolation from 'Integral' to 'Double' (hence: @ID@).
linearInterpolateID :: Integral int => (int, int) -> (Double, Double) -> int -> Double
linearInterpolateID ab xy t = linearInterpolate (both fromIntegral ab) xy (fromIntegral t)

-- | Linear interpolation from 'Integral' to 'Integral' (hence: @II@).
linearInterpolateII :: (Integral intA, Integral intB) => (intA, intA) -> (intB, intB) -> intA -> intB
linearInterpolateII ab xy t = round (linearInterpolate (both fromIntegral ab) (both fromIntegral xy) (fromIntegral t))

-- | Apply a function to both elements of a tuple.
both :: (a -> b) -> (a, a) -> (b, b)
both f (a,b) = (f a, f b)
{-# INLINE both #-}
