-- | Bezier curves.
--
module Geometry.Bezier
(
    Bezier(..)

    -- * Indexing
    , bezierParametric
    , bezierArcParametric

    -- * Subdividing
    , bezierSubdivideEquiparametric
    , bezierSubdivideEquidistant
    , bezierSubdivideCasteljau

    -- * Interpolation
    , bezierSmoothen

    -- * References
    -- $references
)
where



import           Control.DeepSeq
import           Data.Vector     (Vector, (!))
import qualified Data.Vector     as V

import Geometry.Core
import Geometry.LookupTable.Lookup1
import Numerics.ConvergentRecursion
import Numerics.DifferentialEquation
import Numerics.Integrate
import Numerics.LinearEquationSystem



-- $setup
-- >>> import Draw
-- >>> import qualified Graphics.Rendering.Cairo as C



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
    boundingBox bezier@(Bezier start _ _ end) = boundingBox ([start, end] ++ [bezierParametric bezier t | t <- extremalTs])
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
-- \[
-- \text{bezierParametric}_{\mathbf a,\mathbf b,\mathbf c,\mathbf d}(t) = (1-t)^3\,\mathbf a + 3 (1-t)^2 t\,\mathbf b + 3 (1-t) t^2\,\mathbf c + t^3\,\mathbf d
-- \]
bezierParametric
  :: Bezier
  -> Double -- ^ \[0..1] = [start..end]
  -> Vec2
bezierParametric (Bezier a b c d) t
  =      (1-t)^3     *. a
    +. 3*(1-t)^2*t   *. b
    +. 3*(1-t)  *t^2 *. c
    +.           t^3 *. d

-- | First derivative of a 'Bezier' curve, i.e. its velocity vector.
--
-- \[
-- \text{bezierParametric}'_{\mathbf a,\mathbf b,\mathbf c,\mathbf d}(t) = -3(1-t)^2\,\mathbf a + (3+t(-12+9t))\,\mathbf b + (6-9t)t\,\mathbf c + 3t^2\,\mathbf d
-- \]
bezierParametric'
  :: Bezier
  -> Double -- ^ \[0..1] = [start..end]
  -> Vec2
bezierParametric' (Bezier a b c d) t
  =    (-3*(1-t)^2)    *. a
    +. (3+t*(-12+9*t)) *. b
    +. ((6-9*t)*t)     *. c
    +. (3*t^2)         *. d

-- | Second derivative of a 'Bezier' curve, i.e. its acceleration vector.
_bezierParametric''
  :: Bezier
  -> Double -- ^ \[0..1] = [start..end]
  -> Vec2
_bezierParametric'' (Bezier a b c d) t
  =    (6-6*t)         *. a
    +. (-12+18*t)      *. b
    +. (6-18*t)        *. c
    +. (6*t)           *. d

-- | Estimate the length of a 'Bezier' curve by approximating it with a number of segments.
--
-- The number of segments doesn’t need to be very high: 16 is already plenty for most curves.
bezierLength
    :: Bezier
    -> Double
bezierLength bezier = retryExponentiallyUntilPrecision (integrateSimpson13 f 0 1) 1e-6
  where
    f t = norm (bezierParametric' bezier t)



-- | Trace a 'Bezier' curve with a number of points, using the polynomial curve
-- parameterization. This is very fast, but leads to unevenly spaced points.
--
-- For subdivision by arc length, use 'bezierSubdivideEquidistant'.
bezierSubdivideEquiparametric
    :: Int
    -> Bezier
    -> [Vec2]
bezierSubdivideEquiparametric n bz = map (bezierParametric bz) points
  where
    points = map (\x -> fromIntegral x / fromIntegral (n-1)) [0..n-1]

-- | Trace a 'Bezier' curve with a number of evenly spaced points by arc length.
-- This is much more expensive than 'bezierSubdivideEquiparametric', but may be desirable for
-- aesthetic purposes.
--
-- Here it is alongside 'bezierSubdivideEquiparametric':
--
-- <<docs/haddock/Geometry/Bezier/subdivide_s_t_comparison.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Bezier/subdivide_s_t_comparison.svg" 300 150 $ do
--     let curve = let curveRaw = transform (rotate (deg (-30))) (Bezier (Vec2 0 0) (Vec2 1 5) (Vec2 2.5 (-1)) (Vec2 3 3))
--                     fitToBox = transform (transformBoundingBox curveRaw (Vec2 10 10, Vec2 290 90) (TransformBBSettings FitWidthHeight IgnoreAspect FitAlignCenter))
--                 in fitToBox curveRaw
--         evenlySpaced = bezierSubdivideEquidistant 16 curve
--         unevenlySpaced = bezierSubdivideEquiparametric 16 curve
--         offsetBelow :: Transform geo => geo -> geo
--         offsetBelow = transform (translate (Vec2 0 50))
--     cairoScope $ do
--         setColor $ mathematica97 1
--         sketch curve
--         C.stroke
--         sketch (offsetBelow curve)
--         C.stroke
--     for_ (zip evenlySpaced unevenlySpaced) $ \(e, u') -> do
--         let u = offsetBelow u'
--         let circle p = sketch (Circle p 3) >> C.stroke
--             connect p q = do
--                 let line = resizeLineSymmetric (*0.8) (Line p q)
--                 sketch line
--                 C.stroke
--         cairoScope (setColor (mathematica97 0) >> circle e)
--         cairoScope (setColor (mathematica97 3) >> circle u)
--         cairoScope (setColor (black `withOpacity` 0.2) >> connect e u)
-- :}
-- Generated file: size 17KB, crc32: 0x7c147951
bezierSubdivideEquidistant :: Int -> Bezier -> [Vec2]
bezierSubdivideEquidistant n bz = map bezier distances
  where

    -- The step width should correlate with the length of the curve to get a decent
    -- RK estimator. This allows both large and tiny curves to be subdivided well.
    -- Increasing this to beyond 2^10 shows only pixel-wide changes, if even.
    bezier = bezierArcParametric_ode bz (len / 2^10)
    len = bezierLength bz

    distances :: [Double]
    distances = [fromIntegral i * len/fromIntegral (n-1) | i <- [0..n-1]]

-- | Get the position on a Bezier curve as a fraction of its length, via solving a
-- differential equation. This is /much/ more expensive to compute than 'bezierParametric'.
--
-- This caches the internal LUT when partially applied, so that the following will
-- only compute it once for repeated lookups:
--
-- @
-- let walkOnBezier = 'bezierArcParametric' bezier 0.01
-- 'print' [walkOnBezier d | d <- [0, 0.1 .. 5]]
-- @
bezierArcParametric :: Bezier -> Double -> Double -> Vec2
bezierArcParametric = bezierArcParametric_ode

-- There’s also another method to do this using Newton’s method, detialed in the
-- paper (see references on bezier subdivision).
bezierArcParametric_ode
    :: Bezier
    -> Double -- ^ Precision parameter (smaller is more precise but slower).
    -> Double -- ^ Distance to walk on the curve. Clips (stops at the end) when asked to »walk too far«.
    -> Vec2   -- ^ Point at that distance
bezierArcParametric_ode bz ds
  = let lut = s_to_t_lut_ode bz ds
    in \s -> let t = lookupInterpolated lut s
             in bezierParametric bz t

-- | S⇆T lookup table for a Bezier curve
--
-- We do not explicitly add the end point, so too big of a step width will
-- overshoot the end and be cut off. This can be remedied by a small enough step size ;-)
s_to_t_lut_ode
    :: Bezier
    -> Double -- ^ ODE solver step width. Correlates with result precision/length.
    -> LookupTable1 Double Double -- ^ Lookup table
s_to_t_lut_ode bz ds = LookupTable1 (sol_to_vec sol)
  where
    sol_to_vec = V.fromList . takeWhile (\(_s, t) -> t <= 1)

    sol = rungeKuttaConstantStep dt_ds t0 s0 ds

    dt_ds _s t = 1 / norm (bezierParametric' bz t)

    t0 = 0
    s0 = 0

-- | Approximage a Bezier curve with line segments up to a certain precision, using
-- relatively few points.
--
-- The idea behind Casteljau subdivision is that each Bézier curve can be exactly
-- subdivided into two Bézier curves (of same degree). This is done recursively, in
-- this implementation (and commonly) in the middle of the curve. Once a curve
-- segment is flat enough (given by the tolerance parameter), it is simply rendered
-- as a line.
--
-- The algorithm is based on
-- [an excellent blogpost](https://web.archive.org/web/20180307160123/http://antigrain.com/research/adaptive_bezier/index.html)
-- that is sadly only available on the internet archive these days.
--
-- <<docs/haddock/Geometry/Bezier/bezierSubdivideCasteljau.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Bezier/bezierSubdivideCasteljau.svg" 500 330 $ do
--     let curve = let curveRaw = transform (rotate (deg (-30))) (Bezier (Vec2 0 0) (Vec2 1 5) (Vec2 2.5 (-1)) (Vec2 3 3))
--                     fitToBox = transform (transformBoundingBox curveRaw (shrinkBoundingBox 10 [zero, Vec2 500 200]) (TransformBBSettings FitWidthHeight IgnoreAspect FitAlignCenter))
--                 in fitToBox curveRaw
--         paintOffset = Vec2 0 30
--     for_ (zip [0..] [50,25,10,2]) $ \(i, tolerance) -> cairoScope $ do
--         let points = bezierSubdivideCasteljau tolerance (transform (translate (fromIntegral i *. paintOffset)) curve)
--         setColor (mathematica97 i)
--         C.setLineWidth 2
--         sketch (Polyline points) >> C.stroke
--         for_ points $ \p -> sketch (Circle p 3) >> C.fill
--     cairoScope $ do
--         C.setLineWidth 3
--         setColor black
--         sketch (transform (translate (4*.paintOffset)) curve)
--         C.stroke
-- :}
-- Generated file: size 20KB, crc32: 0x679b311c
bezierSubdivideCasteljau :: Double -> Bezier -> [Vec2]
bezierSubdivideCasteljau tolerance curve@(Bezier pFirst _ _ _) = pFirst : go curve
  where
    go (Bezier p1 p2@(Vec2 x2 y2) p3@(Vec2 x3 y3) p4@(Vec2 x4 y4)) =
        let
            p12   = (p1   +. p2  ) /. 2
            p23   = (p2   +. p3  ) /. 2
            p34   = (p3   +. p4  ) /. 2
            p123  = (p12  +. p23 ) /. 2
            p234  = (p23  +. p34 ) /. 2
            p1234 = (p123 +. p234) /. 2

            dp@(Vec2 dx dy) = p4 -. p1

            -- d2, d3 are the distance from p2, p3 from the line
            -- connecting p1 and p4. A curve is flat when those
            -- two are short together.
            d2 = abs ((x2-x4)*dy - (y2-y4)*dx)
            d3 = abs ((x3-x4)*dy - (y3-y4)*dx)
            curveIsFlat = (d2 + d3)*(d2 + d3) < tolerance^2 * normSquare dp
        in if curveIsFlat
            then -- We return only the last point so we don’t get duplicate
                 -- points for each start+end of adjacent curves.
                 -- The very first point is forgotten by the
                [p4]
            else go (Bezier p1 p12 p123 p1234)
                 ++
                 go (Bezier p1234 p234 p34 p4)

-- | Smoothen a number of points by putting a Bezier curve between each pair.
-- Useful to e.g. make a sketch nicer, or interpolate between points of a crude
-- solution to a differential equation.
--
-- If the first and last point are identical, assume the trajectory is closed, and
-- smoothly interpolate between beginning and end as well, yielding a nice loop.
--
-- <<docs/haddock/Geometry/Bezier/bezierSmoothen.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Bezier/bezierSmoothen.svg" 400 300 $ do
--     let points = [ Vec2 100 275, Vec2 150 125, Vec2 300 275, Vec2 350 75
--                  , Vec2 250 50, Vec2 75 75, Vec2 75 50, Vec2 225 100
--                  , Vec2 100 275 ]
--         prettyBezier bezier@(Bezier p0 p1 p2 p3) = do
--             cairoScope $ do
--                 setColor black
--                 sketch bezier
--                 C.stroke
--             cairoScope $ do
--                 setColor (mathematica97 0)
--                 sketch (Circle p1 4) >> C.fill
--                 sketch (Line p0 p1) >> C.stroke
--             cairoScope $ do
--                 setColor (mathematica97 1)
--                 sketch (Circle p2 4) >> C.fill
--                 sketch (Line p3 p2) >> C.stroke
--             cairoScope $ do
--                 sketch (Circle p0 5)
--                 setColor (mathematica97 3)
--                 C.fillPreserve
--                 setColor black
--                 C.stroke
--     C.setLineWidth 2
--     for_ (bezierSmoothenLoop points) prettyBezier
-- :}
-- Generated file: size 15KB, crc32: 0x9f26361e
bezierSmoothen :: Sequential vector => vector Vec2 -> Vector Bezier
bezierSmoothen vecSequence
    | V.head vec == V.last vec = bezierSmoothenLoop vec
    | otherwise = bezierSmoothenOpen vec
    where vec = toVector vecSequence

-- | Smoothen a number of points by putting a Bezier curve between each pair,
-- assuming the curve is open (i.e. the last and first point have nothing to do
-- with each other).
bezierSmoothenOpen :: Sequential vector => vector Vec2 -> Vector Bezier
bezierSmoothenOpen pointsSequence = V.zipWith4 Bezier points controlPointsStart controlPointsEnd (V.tail points)
  where
    points = toVector pointsSequence
    n = V.length points - 1

    controlPointsStart =
        let low   = lowerDiagonal (n-1)
            diag  = diagonal      n
            upper = upperDiagonal (n-1)
            rhs   = target        n points
        in solveTridiagonal low diag upper rhs
    controlPointsEnd = V.generate (V.length controlPointsStart) $ \i -> case () of
        _ | i == n-1 -> (points ! n +. controlPointsStart ! (n-1)) /. 2
          | otherwise -> 2 *. (points ! (i+1)) -. controlPointsStart ! (i+1)

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
bezierSmoothenLoop :: Sequential vector => vector Vec2 -> Vector Bezier
bezierSmoothenLoop pointsSequence
    | length points <= 2 = V.empty
    | otherwise =
        -- The idea is this: we can artificially lengthen a closed trajectory by
        -- wrapping it onto itself. We then interpolate it as if it was open, and later
        -- forget the end parts again. In the overlap, we get a smooth transition.
        let opened = V.tail points
            openedWithAppendix = opened <> V.take 4 opened
        in V.slice 2 (V.length points-1) (bezierSmoothenOpen openedWithAppendix)
    where
      points = toVector pointsSequence
