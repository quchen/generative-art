-- |
-- -- * References
--
-- * Moving Along a Curve with Specified Speed (2019)
-- by David Eberly
-- https://www.geometrictools.com/Documentation/MovingAlongCurveSpecifiedSpeed.pdf
module Geometry.ArcLengthParameterization where

import Geometry
import Data.List
import Geometry.Processes.DifferentialEquation
import qualified Data.Vector as V

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

-- | Point on a 'Bezier' curve.
--
-- The suffix @T@ indicates the »simple formula« parameterization,
-- which makes this curve easy to compute.
bezierT
  :: Bezier
  -> T Double -- ^ [0..1] = [start..end]
  -> Vec2
bezierT (Bezier a b c d) (T t)
  =      (1-t)^3     *. a
    +. 3*(1-t)^2*t   *. b
    +. 3*(1-t)  *t^2 *. c
    +.           t^3 *. d

-- | First derivative of a 'Bezier' curve, i.e. its velocity vector.
--
-- The suffix @T@ indicates the »simple formula« parameterization,
-- which makes this curve easy to compute.
bezierT'
  :: Bezier
  -> T Double -- ^ [0..1] = [start..end]
  -> Vec2
bezierT' (Bezier a b c d) (T t)
  =    (-3*(1-t)^2)    *. a
    +. (3+t*(-12+9*t)) *. b
    +. ((6-9*t)*t)     *. c
    +. (3*t^2)         *. d

-- | Second derivative of a 'Bezier' curve, i.e. its acceleration vector.
--
-- The suffix @T@ indicates the »simple formula« parameterization,
-- which makes this curve easy to compute.
bezierT''
  :: Bezier
  -> T Double -- ^ [0..1] = [start..end]
  -> Vec2
bezierT'' (Bezier a b c d) (T t)
  =    (6-6*t)         *. a
    +. (-12+18*t)      *. b
    +. (6-18*t)        *. c
    +. (6*t)           *. d

-- | Estimate the length of a 'Bezier' curve by approximating it with a number of segments.
--
-- The number of segments doesn’t need to be very high: 16 is already plenty for most curves.
bezierLength
    :: Int      -- ^ Number of segments
    -> Bezier   -- ^ Curve
    -> Distance
bezierLength n bezier = Distance (integrateSimpson13 f 0 1 n)
  where
    f t = let Distance d = norm (bezierT' bezier (T t)) in d

-- | Numerical integration with the midpoint method.
integrateMidpoint
    :: Fractional a
    => (a -> a) -- ^ f
    -> a        -- ^ a
    -> a        -- ^ b
    -> Int      -- ^ Number of interval subdivisions
    -> a        -- ^ ∫_a^b f(t) dt
integrateMidpoint f a b n =
    h * ( f a / 2
        + sum' [f (a + fromIntegral k*h) | k <- [1..n-1]]
        + f b / 2
        )
  where
    h = (b-a)/fromIntegral n
    sum' = foldl' (+) 0

-- | Numerical integration with Simpson’s ⅓ rule.
integrateSimpson13
    :: Fractional a
    => (a -> a) -- ^ f
    -> a        -- ^ a
    -> a        -- ^ b
    -> Int      -- ^ Number of interval subdivisions
    -> a        -- ^ ∫_a^b f(t) dt
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
    sum' = foldl' (+) 0

-- | Trace a 'Bezier' curve with a number of points, using the polynomial curve
-- parameterization. This is very fast, but leads to unevenly spaced points.
--
-- For subdivision by arc length, use 'bezierSubdivideS'.
bezierSubdivideT
    :: Int
    -> Bezier
    -> [Vec2]
bezierSubdivideT n bz = map (bezierT bz) points
  where
    points :: [T Double]
    points = map (\x -> fromIntegral x / fromIntegral (n-1)) [0..n-1]

-- | Trace a 'Bezier' curve with a number of evenly spaced points by arc length.
-- This is much more expensive than 'bezierSubdivideT', but may be desirable for
-- aesthetic purposes.
bezierSubdivideS :: Int -> Bezier -> [Vec2]
bezierSubdivideS n bz = map bezier distances
  where
    bezier = bezierS_ode bz 0.01
    Distance len = bezierLength 16 bz

    distances :: [Distance]
    distances = [Distance (fromIntegral i * len/fromIntegral (n-1)) | i <- [0..n-1]]

-- | Get the position on a Bezier curve as a fraction of its length. This is _much_
-- more expensive to compute than 'bezierT'.
--
-- This approach inverts an integral using Newton’s method. For a different
-- approach, see 'bezierS_ode'.
bezierS_integral :: Bezier -> Double -> Vec2
bezierS_integral bz s = error "TODO"
  where
    _t0 = let Distance approximateLength = bezierLength 10 bz
              tMin = 0
              tMax = 1
          in tMin + s/approximateLength *(tMax-tMin)

-- | Get the position on a Bezier curve as a fraction of its length, via solving a
-- differential equation. This is _much_ more expensive to compute than 'bezierT'.
--
-- Multiple calls to this function use a cached lookup table, so that the expensive
-- step has to be done only once here:
--
-- @
-- let s = bezierS_ode bezier 0.01
-- print [s (Distance d) | d <- [0, 0.1 .. 5]]
-- @
bezierS_ode
    :: Bezier
    -> Double   -- ^ Precision parameter (smaller is more precise but slower).
    -> Distance -- ^ Distance to walk on the curve. Clips (stops at the end) when asked to »walk too far«.
    -> Vec2     -- ^ Point at that distance
bezierS_ode bz ds
  = let lut = s_to_t_lut_ode bz ds
    in \(Distance s) -> let t = s_to_t lut (S s)
                        in bezierT bz t



-- | Safety newtype wrapper because it’s super confusing that Runge-Kutta commonly
-- has t for time, but here we have s(t) where s corredponds to time in RK.
--
-- 'T' is used for simple Bezier parametrization, 'S' for curve-length based.
newtype T a = T a deriving (Eq, Ord, Show)

instance Num a => Num (T a) where
    T a + T b = T (a+b)
    T a - T b = T (a-b)
    T a * T b = T (a*b)
    abs (T a) = T (abs a)
    signum (T a) = T (signum a)
    negate (T a) = T (negate a)
    fromInteger i = T (fromInteger i)

instance Fractional a => Fractional (T a) where
    T a / T b = T (a/b)
    recip (T a) = T (recip a)
    fromRational r = T (fromRational r)

instance VectorSpace a => VectorSpace (T a) where
    T a +. T b = T (a+.b)
    a *. T b = T (a*.b)
    T a -. T b = T (a-.b)

-- | See 'T'.
--
-- 'T' is used for simple Bezier parametrization, 'S' for curve-length based.
newtype S a = S a deriving (Eq, Ord, Show)

-- | Find t(s) from a LUT using binary search. Clips for out-of-range values.
s_to_t :: V.Vector (S Double, T Double) -> S Double -> T Double
s_to_t lut s
    | V.length lut == 0    = error "s_to_t: empty lookup table"
    | V.length lut == 1    = snd (V.head lut)
    | s < fst (V.head lut) = snd (V.head lut)
    | s > fst (V.last lut) = snd (V.last lut)
s_to_t lut searchS
  = let middleIx = V.length lut `div` 2
        (lutSmaller, (pivotS, pivotT), lutGreater) = divideOnIndex middleIx lut
    in case compare pivotS searchS of
        LT -> s_to_t lutGreater searchS
        EQ -> pivotT
        GT -> s_to_t lutSmaller searchS

-- | Split a 'V.Vector' at an index into the slice before, the value at the index, and the slice after.
--
-- >>> divideOnIndex 0 (V.fromList [0..10])
-- ([],0,[1,2,3,4,5,6,7,8,9,10])
--
-- divideOnIndex 3 (V.fromList [0..10])
-- ([0,1,2],3,[4,5,6,7,8,9,10])
--
-- divideOnIndex 10 (V.fromList [0..10])
-- ([0,1,2,3,4,5,6,7,8,9],10, [])
divideOnIndex :: Int -> V.Vector a -> (V.Vector a, a, V.Vector a)
divideOnIndex ix vec
    | ix >= V.length vec = error ("divideOnIndex: index out of bounds. " ++ "i = " ++ show ix ++ ", length = " ++ show (V.length vec))
divideOnIndex ix vec
  = let (before, after') = V.splitAt ix vec
        Just (pivot, after) = V.uncons after'
    in (before, pivot, after)

-- | S⇆T lookup table for a Bezier curve
s_to_t_lut_ode
    :: Bezier
    -> Double -- ^ ODE solver step width. Correlates with result precision/length.
    -> V.Vector (S Double, T Double) -- ^ Values increase with vector index, enabling binary search.
s_to_t_lut_ode bz ds = sol_to_vec sol
  where
    sol_to_vec = V.map (\(s, tt) -> (S s, tt)) . V.fromList . takeWhile (\(_s, T t) -> t <= 1)

    sol = rungeKuttaConstantStep dt_ds t0 s0 ds

    dt_ds _s t = T (1 / let Distance d = norm (bezierT' bz t) in d)

    t0 = T 0
    s0 = 0
