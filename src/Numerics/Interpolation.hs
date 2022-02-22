-- | Interpolate between values.
module Numerics.Interpolation (
      linearInterpolate

    , linearInterpolateDI
    , linearInterpolateID
    , linearInterpolateII
) where



import Algebra.VectorSpace



-- | Linearly interpolate the interval \([a,b]\) to \([x,y]\). In computer graphics
-- lingo, this is often known as /lerp/.
--
-- \[
-- f(t) = x + \frac{y-x}{b-a} (t-a)
-- \]
--
-- >>> linearInterpolate (0,1) (10,20) 0.5
-- 15
linearInterpolate :: VectorSpace vec => (Double, Double) -> (vec, vec) -> Double -> vec
linearInterpolate (a, b) (x, y) t = x +. ((t-a) / (b-a))*.(y-.x)

-- | Linear interpolation from 'Double' to 'Integral' (hence: @DI@).
linearInterpolateDI :: Integral int => (Double, Double) -> (int, int) -> Double -> int
linearInterpolateDI ab xy = round . linearInterpolate ab (doubles xy)

-- | Linear interpolation from 'Integral' to 'Double' (hence: @ID@).
linearInterpolateID :: Integral int => (int, int) -> (Double, Double) -> int -> Double
linearInterpolateID ab xy t = linearInterpolate (doubles ab) xy (fromIntegral t)

-- | Linear interpolation from 'Integral' to 'Integral' (hence: @II@).
linearInterpolateII :: (Integral intA, Integral intB) => (intA, intA) -> (intB, intB) -> intA -> intB
linearInterpolateII ab xy t = round (linearInterpolate (doubles ab) (doubles xy) (fromIntegral t))

doubles :: Integral a => (a, a) -> (Double, Double)
doubles (a,b) = (fromIntegral a, fromIntegral b)
