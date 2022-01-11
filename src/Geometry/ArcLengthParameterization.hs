module Geometry.ArcLengthParameterization where

import Geometry
import Data.List

newtonStep
    :: Double             -- ^ t_n
    -> (Double -> Double) -- ^ f(t_n)
    -> (Double -> Double) -- ^ f'(t_n)
    -> Double             -- ^ t_{n+1}
newtonStep t f f' = t - f t/f' t

-- -- | Iterate the Newton method a number of times
findRoot
    :: Int                -- ^ Number of iterations @n@
    -> Double             -- ^ Initial guess
    -> (Double -> Double) -- ^ f
    -> (Double -> Double) -- ^ f'
    -> Double             -- ^ Root approximation after @n@ steps
findRoot n t0 f f' = iterate (\t -> newtonStep t f f') t0 !! n

-- | Underlying function of a 'Bezier' curve, accessed by an arbitrary parameter.
bezierT :: VectorSpace vec => Bezier vec -> Double -> vec
bezierT (Bezier a b c d) t
  =      (1-t)^3     *. a
    +. 3*(1-t)^2*t   *. b
    +. 3*(1-t)  *t^2 *. c
    +.           t^3 *. d

-- | First derivative of a 'Bezier' curve
bezierT' :: VectorSpace vec => Bezier vec -> Double -> vec
bezierT' (Bezier a b c d) t
  =    (-3*(1-t)^2)    *. a
    +. (3+t*(-12+9*t)) *. b
    +. ((6-9*t)*t)     *. c
    +. (3*t^2)         *. d

-- | Second derivative of a 'Bezier' curve
bezierT'' :: VectorSpace vec => Bezier vec -> Double -> vec
bezierT'' (Bezier a b c d) t
  =    (6-6*t)         *. a
    +. (-12+18*t)      *. b
    +. (6-18*t)        *. c
    +. (6*t)           *. d

-- | Estimate the length of a 'Bezier' curve by linear subdivision with a number of
-- segments.
bezierLength :: Int -> Bezier Vec2 -> Distance
bezierLength segments bz
  = let points = Geometry.ArcLengthParameterization.bezierSubdivide segments bz
        lineSegments = zipWith Line points (tail points)
        segmentLengths = map lineLength lineSegments
    in foldl' (+.) (Distance 0) segmentLengths

bezierSubdivide :: VectorSpace vec => Int -> Bezier vec -> [vec]
bezierSubdivide n bz = map (bezierT bz) points
  where
    points :: [Double]
    points = map (\x -> fromIntegral x / fromIntegral (n-1)) [0..n-1]

-- | Get the position on a Bezier curve as a fraction of its length. This is _much_
-- more expensive to compute than 'bezierT'.
bezierNatural :: Bezier Vec2 -> Double -> Vec2
bezierNatural bz s = error "TODO"
  where
    t0 = let Distance approximateLength = bezierLength 10 bz
             tMin = 0
             tMax = 1
         in tMin + s/approximateLength *(tMax-tMin)
