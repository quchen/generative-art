-- | Interpolate between values.
module Numerics.Interpolation (
      lerp

    , lerpDI
    , lerpID
    , lerpII
) where



import Algebra.VectorSpace



-- | Linearly interpolate the interval \([a,b]\) to \([x,y]\). In computer graphics
-- lingo, this is often known as /lerp/.
--
-- \[
-- f(t) = x + \frac{y-x}{b-a} (t-a)
-- \]
--
-- >>> lerp (0,1) (10,20) 0.5
-- 15.0
lerp :: VectorSpace vec => (Double, Double) -> (vec, vec) -> Double -> vec
lerp (a, b) (x, y) t = x +. ((t-a) / (b-a))*.(y-.x)

-- | Linear interpolation from 'Double' to 'Integral' (hence: @DI@).
lerpDI :: Integral int => (Double, Double) -> (int, int) -> Double -> int
lerpDI ab xy = round . lerp ab (doubles xy)

-- | Linear interpolation from 'Integral' to 'Double' (hence: @ID@).
lerpID :: Integral int => (int, int) -> (Double, Double) -> int -> Double
lerpID ab xy t = lerp (doubles ab) xy (fromIntegral t)

-- | Linear interpolation from 'Integral' to 'Integral' (hence: @II@).
lerpII :: (Integral intA, Integral intB) => (intA, intA) -> (intB, intB) -> intA -> intB
lerpII ab xy t = round (lerp (doubles ab) (doubles xy) (fromIntegral t))

doubles :: Integral a => (a, a) -> (Double, Double)
doubles (a,b) = (fromIntegral a, fromIntegral b)
