-- | Bezier curves.
--
module Geometry.Bezier
(
    Bezier(..)

    -- == Indexing
    -- , bezierT
    , bezierS

    -- == Subdividing
    , bezierSubdivideT
    , bezierSubdivideS

    -- * References
    -- $references
)
where

import Data.List
import Geometry.Core
import Geometry.Processes.DifferentialEquation
import qualified Data.Vector as V

-- | Cubic Bezier curve, defined by start, first/second control points, and end.
data Bezier = Bezier Vec2 Vec2 Vec2 Vec2 deriving (Eq, Ord, Show)

instance Transform Bezier where
    transform t (Bezier a b c d) = Bezier
        (transform t a)
        (transform t b)
        (transform t c)
        (transform t d)

instance HasBoundingBox Bezier where
    boundingBox bezier@(Bezier start _ _ end) = boundingBox ([start, end] ++ [bezierT bezier (T t) | t <- extremalTs])
      where

        -- Alg idea: find the roots of the Bezier’s first derivative, collect
        -- points where x/y components are extremal. Hastily written ugly code

        extremalTs = filter (\t -> t >=0 && t <= 1) (extremalT bezier)

        extremalT (Bezier a b c d) = solveQuadratic (x a') (x b') (x c') ++ solveQuadratic (y a') (y b') (y c')
          where
            -- Coefficients of the first derivative polynomial of the Bezier curve
            a' = ((-3) *.a) +. (9*.b) -. 9*.c +. 3*.d
            b' = 6*.a -. 12*.b +. 6*.c
            c' = (-3)*.a +. 3*.b

            x (Vec2 x' _) = x'
            y (Vec2 _ y') = y'

        solveQuadratic a b c = case compare discriminant 0 of
            LT -> []
            EQ -> [-b/(2*a)]
            GT -> [ (-b + sqrt discriminant) / (2*a), (-b - sqrt discriminant) / (2*a)]
          where
            discriminant = b^2 - 4 * a * c

-- | Point on a 'Bezier' curve.
--
-- The suffix @T@ indicates the »simple formula« parameterization,
-- which makes this curve easy to compute.
bezierT
  :: Bezier
  -> T Double -- ^ \[0..1] = [start..end]
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
  -> T Double -- ^ \[0..1] = [start..end]
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
  -> T Double -- ^ \[0..1] = [start..end]
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
    :: Bezier   -- ^ Curve
    -> Distance
bezierLength bezier = Distance (integrateConvergently (integrateSimpson13 f 0 1) 1e-6)
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

integrateConvergently
    :: (Fractional a, Ord a)
    => (Int -> a) -- ^ Integration function: f a b n
    -> a          -- ^ Relative error threshold between iterations before committing
    -> a          -- ^ Result
integrateConvergently integrateSteps threshold
  = let results = [integrateSteps steps | steps <- map (2^) [1..]]
        closeEnoughPair (x,y) = (x-y)/x < threshold
        Just (_good, evenBetter) = find closeEnoughPair (zip results (tail results))
    in evenBetter

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

    -- The step width should correlate with the length of the curve to get a decent
    -- RK estimator. This allows both large and tiny curves to be subdivided well.
    -- Increasing this to beyond 2^10 shows only pixel-wide changes, if even.
    bezier = bezierS_ode bz (len / 2^10)
    Distance len = bezierLength bz

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
    _t0 = let Distance approximateLength = bezierLength bz
              tMin = 0
              tMax = 1
          in tMin + s/approximateLength *(tMax-tMin)

-- | Get the position on a Bezier curve as a fraction of its length, via solving a
-- differential equation. This is /much/ more expensive to compute than 'bezierT'.
--
-- Multiple calls to this function use a cached lookup table, so that the expensive
-- step has to be done only once here:
--
-- @
-- let s = 'bezierS' bezier 0.01
-- 'print' [s ('Distance' d) | d <- [0, 0.1 .. 5]]
-- @
bezierS :: Bezier -> Double -> Distance -> Vec2
bezierS = bezierS_ode

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

instance Num a => Num (S a) where
    S a + S b = S (a+b)
    S a - S b = S (a-b)
    S a * S b = S (a*b)
    abs (S a) = S (abs a)
    signum (S a) = S (signum a)
    negate (S a) = S (negate a)
    fromInteger i = S (fromInteger i)

-- | Find t(s) from a LUT using binary search, interpolating linearly around the
-- search result. Clips for out-of-range values.
s_to_t :: V.Vector (S Double, T Double) -> S Double -> T Double
s_to_t lut needle = interpolate (search 0 (V.length lut))
  where
    -- Binary search between lo and hi
    search lo hi
        | lo >= mid = mid
        | hi <= mid = mid
        | otherwise
          = let (pivotS, _) = lut V.! mid
            in case compare pivotS needle of
                LT -> search mid hi
                EQ -> mid
                GT -> search lo mid
      where
        mid = (hi+lo) `div` 2

    -- Look at left/right neighbours. If they’re there and the needle is between it
    -- and the found pivot index, then linearly interpolate the result value
    -- between them.
    interpolate pivot = case (lut V.!? (pivot-1), lut V.! pivot, lut V.!? (pivot+1)) of
        -- Interpolate between pivot and left neighbour?
        (Just (leftS, leftT), (pivotS, pivotT), _)
            | between (leftS, pivotS) needle -> linearInterpolate (leftS, pivotS) (leftT, pivotT) needle
        -- Interpolate between pivot and right neighbour?
        (_, (pivotS, pivotT), Just (rightS, rightT))
            | between (pivotS, rightS) needle -> linearInterpolate (pivotS, rightS) (pivotT, rightT) needle
        -- Fallback: don’t interpolate
        (_, (_, pivotT), _) -> pivotT

    -- Linearly interpolate the interval [a,b] to [x,y] and get the point t
    linearInterpolate :: (S Double, S Double) -> (T Double, T Double) -> S Double -> T Double
    linearInterpolate (S a, S b) (T x, T y) (S s)
        = T (x + (y-x)/(b-a) * (s-a))

    between :: Ord a => (a, a) -> a -> Bool
    between (a,b) x = min a b < x && x < max a b


-- | S⇆T lookup table for a Bezier curve
--
-- We do not explicitly add the end point, so too big of a step width will
-- overshoot the end and be cut off. This can be remedied by a large step size ;-)
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

-- $references
--
-- * Moving Along a Curve with Specified Speed (2019)
-- by David Eberly
-- https://www.geometrictools.com/Documentation/MovingAlongCurveSpecifiedSpeed.pdf
