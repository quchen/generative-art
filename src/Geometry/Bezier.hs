-- | Bezier curves.
--
module Geometry.Bezier
(
    Bezier(..)

    -- * Indexing
    , bezierT
    , bezierS

    -- * Subdividing
    , bezierSubdivideT
    , bezierSubdivideS

    -- * Interpolation
    , bezierSmoothen
    , bezierSmoothenLoop

    -- * References
    -- $references
)
where

import           Control.DeepSeq
import           Data.Vector                   (Vector, (!))
import qualified Data.Vector                   as V

import           Geometry.Core
import           Geometry.LUT
import           Numerics.ConvergentRecursion
import           Numerics.DifferentialEquation
import           Numerics.Integrate
import           Numerics.LinearEquationSystem

-- | Cubic Bezier curve, defined by start, first/second control points, and end.
data Bezier = Bezier !Vec2 !Vec2 !Vec2 !Vec2 deriving (Eq, Ord, Show)

instance NFData Bezier where rnf _ = ()

instance Transform Bezier where
    transform t (Bezier a b c d) = Bezier
        (transform t a)
        (transform t b)
        (transform t c)
        (transform t d)

instance HasBoundingBox Bezier where
    boundingBox bezier@(Bezier start _ _ end) = boundingBox ([start, end] ++ [bezierT bezier t | t <- extremalTs])
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
--
-- \[
-- \text{bezierT}_{\mathbf a,\mathbf b,\mathbf c,\mathbf d}(t) = (1-t)^3\,\mathbf a + 3 (1-t)^2 t\,\mathbf b + 3 (1-t) t^2\,\mathbf c + t^3\,\mathbf d
-- \]
bezierT
  :: Bezier
  -> Double -- ^ \[0..1] = [start..end]
  -> Vec2
bezierT (Bezier a b c d) t
  =      (1-t)^3     *. a
    +. 3*(1-t)^2*t   *. b
    +. 3*(1-t)  *t^2 *. c
    +.           t^3 *. d

-- | First derivative of a 'Bezier' curve, i.e. its velocity vector.
--
-- The suffix @T@ indicates the »simple formula« parameterization,
-- which makes this curve easy to compute.
--
-- \[
-- \text{bezierT}'_{\mathbf a,\mathbf b,\mathbf c,\mathbf d}(t) = -3(1-t)^2\,\mathbf a + (3+t(-12+9t))\,\mathbf b + (6-9t)t\,\mathbf c + 3t^2\,\mathbf d
-- \]
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
_bezierT''
  :: Bezier
  -> T Double -- ^ \[0..1] = [start..end]
  -> Vec2
_bezierT'' (Bezier a b c d) (T t)
  =    (6-6*t)         *. a
    +. (-12+18*t)      *. b
    +. (6-18*t)        *. c
    +. (6*t)           *. d

-- | Estimate the length of a 'Bezier' curve by approximating it with a number of segments.
--
-- The number of segments doesn’t need to be very high: 16 is already plenty for most curves.
bezierLength
    :: Bezier   -- ^ Curve
    -> Double
bezierLength bezier = retryExponentiallyUntilPrecision (integrateSimpson13 f 0 1) 1e-6
  where
    f t = norm (bezierT' bezier (T t))



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
    points = map (\x -> fromIntegral x / fromIntegral (n-1)) [0..n-1]

-- | Trace a 'Bezier' curve with a number of evenly spaced points by arc length.
-- This is much more expensive than 'bezierSubdivideT', but may be desirable for
-- aesthetic purposes.
--
-- Here it is alongside 'bezierSubdivideT':
--
-- <<docs/bezier/1_single_curve.svg>>
bezierSubdivideS :: Int -> Bezier -> [Vec2]
bezierSubdivideS n bz = map bezier distances
  where

    -- The step width should correlate with the length of the curve to get a decent
    -- RK estimator. This allows both large and tiny curves to be subdivided well.
    -- Increasing this to beyond 2^10 shows only pixel-wide changes, if even.
    bezier = bezierS_ode bz (len / 2^10)
    len = bezierLength bz

    distances :: [Double]
    distances = [fromIntegral i * len/fromIntegral (n-1) | i <- [0..n-1]]

-- | Get the position on a Bezier curve as a fraction of its length, via solving a
-- differential equation. This is /much/ more expensive to compute than 'bezierT'.
--
-- This caches the internal LUT when partially applied, so that the following will
-- only compute it once for repeated lookups:
--
-- @
-- let s = 'bezierS' bezier 0.01
-- 'print' [s ('Distance' d) | d <- [0, 0.1 .. 5]]
-- @
bezierS :: Bezier -> Double -> Double -> Vec2
bezierS = bezierS_ode

-- There’s also another method to do this using Newton’s method, detialed in the
-- paper (see references on bezier subdivision).
bezierS_ode
    :: Bezier
    -> Double   -- ^ Precision parameter (smaller is more precise but slower).
    -> Double -- ^ Distance to walk on the curve. Clips (stops at the end) when asked to »walk too far«.
    -> Vec2     -- ^ Point at that distance
bezierS_ode bz ds
  = let lut = s_to_t_lut_ode bz ds
    in \s -> let T t = lookupInterpolated lut (S s)
             in bezierT bz t

-- | S⇆T lookup table for a Bezier curve
--
-- We do not explicitly add the end point, so too big of a step width will
-- overshoot the end and be cut off. This can be remedied by a small enough step size ;-)
s_to_t_lut_ode
    :: Bezier
    -> Double -- ^ ODE solver step width. Correlates with result precision/length.
    -> VLUT (S Double) (T Double) -- ^ Lookup table
s_to_t_lut_ode bz ds = VLUT (sol_to_vec sol)
  where
    sol_to_vec = V.map (\(s, tt) -> (S s, tt)) . V.fromList . takeWhile (\(_s, T t) -> t <= 1)

    sol = rungeKuttaConstantStep dt_ds t0 s0 ds

    dt_ds _s t = T (1 / norm (bezierT' bz t))

    t0 = T 0
    s0 = 0

-- $references
--
-- == Arc length parameterization
--
-- * Moving Along a Curve with Specified Speed (2019)
--   by David Eberly
--   https://www.geometrictools.com/Documentation/MovingAlongCurveSpecifiedSpeed.pdf
--
-- == Smoothening
--
-- * Cubic Bézier Splines
--   by Michael Joost
--   https://www.michael-joost.de/bezierfit.pdf
--
-- * Building Smooth Paths Using Bézier Curves
--   by Stuart Kent
--   https://www.stkent.com/2015/07/03/building-smooth-paths-using-bezier-curves.html

-- | Smoothen a number of points by putting a Bezier curve between each pair.
-- Useful to e.g. make a sketch nicer, or interpolate between points of a crude
-- solution to a differential equation.
--
-- For an input of n+1 points, this will yield n Bezier curves.
--
-- <<docs/interpolation/1_bezier_open.svg>>
bezierSmoothen :: [Vec2] -> [Bezier]
bezierSmoothen points = V.toList (V.zipWith4 Bezier pointsV controlPointsStart controlPointsEnd (V.tail pointsV))
  where
    pointsV = V.fromList points
    n = V.length pointsV - 1

    controlPointsStart =
        let low   = lowerDiagonal (n-1)
            diag  = diagonal      n
            upper = upperDiagonal (n-1)
            rhs   = target        n pointsV
        in solveTridiagonal low diag upper rhs
    controlPointsEnd = V.generate (V.length controlPointsStart) $ \i -> case () of
        _ | i == n-1 -> (pointsV ! n +. controlPointsStart ! (n-1)) /. 2
          | otherwise -> 2 *. (pointsV ! (i+1)) -. controlPointsStart ! (i+1)

upperDiagonal :: Int -> Vector Double
upperDiagonal len = V.replicate len 1

lowerDiagonal :: Int -> Vector Double
lowerDiagonal len = V.generate len $ \i -> case () of
    _ | i == len-1 -> 2
      | otherwise -> 1

diagonal :: Int -> Vector Double
diagonal len = V.generate len $ \i -> case () of
    _ | i == 0     -> 2
      | i == len-1 -> 7
      | otherwise  -> 4

target :: VectorSpace vec => Int -> Vector vec -> Vector vec
target n vertices = V.generate n $ \i -> case () of
    _ | i == 0    ->      vertices ! 0     +. 2 *. vertices ! 1
      | i == n-1  -> 8 *. vertices ! (n-1) +.      vertices ! n
      | otherwise -> 4 *. vertices ! i     +. 2 *. vertices ! (i+1)

-- | Like 'bezierSmoothen', but will smoothly connect the start and the end of the
-- given trajectory as well. (Simply using 'bezierSmoothen' will yield a sharp bend
-- at the line’s origin.)
bezierSmoothenLoop :: [Vec2] -> [Bezier]
bezierSmoothenLoop points = (drop 1 . dropFromEnd 1 . bezierSmoothen) (points ++ take 3 points)

dropFromEnd :: Int -> [b] -> [b]
dropFromEnd n xs = zipWith const xs (drop n xs)
