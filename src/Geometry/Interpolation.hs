module Geometry.Interpolation (
      bezierSmoothen
    , simplifyPath
    , bezierSubdivide
) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Geometry.Core
import Data.Ord

-- | Smoothen a number of points by putting a Bezier curve between each pair.
-- Useful to e.g. make a sketch nicer, or interpolate between points of a crude
-- solution to a differential equation.
--
-- For an input of n+1 points, this will yield n Bezier curves.
--
-- Heavily inspired by
-- https://www.michael-joost.de/bezierfit.pdf
-- https://www.stkent.com/2015/07/03/building-smooth-paths-using-bezier-curves.html
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

-- See https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
-- Translated with blood, sweat and tears from 1-and-2(!!)-based indexing
solveTridiagonal
    :: VectorSpace vec
    => Vector Double -- ^ Lower diagonal, length n-1
    -> Vector Double -- ^ Diagonal, length n
    -> Vector Double -- ^ Upper diagonal, length n-1
    -> Vector vec   -- ^ RHS, length n
    -> Vector vec
solveTridiagonal a b c d
  = let n = V.length d
        ifor = flip V.imap
        c' = ifor c $ \i c_i -> case i of
            0 -> c_i / b!i
            _ -> c_i / (b!i - a!(i-1) * c'!(i-1))
        d' = ifor d $ \i d_i -> case i of
            0 -> d_i /. b!i
            _ -> (d_i -. a!(i-1) *. d'!(i-1))  /.  (b!i - a!(i-1) * c'!(i-1))
        x = ifor d' $ \i d'_i -> case i of
            _ | i == n-1 -> d'_i
            _            -> d'_i -. c'!i *. x!(i+1)
    in x

-- | Simplify a path by dropping unnecessary points. The larger the tolerance
-- distance, the simpler the result will be.
--
-- This is very useful in conjunction with 'bezierSmoothen': first drop the
-- redundancies, then smoothen using Bezier curves again, to yield a result
-- visually similar to the original data, but with a much smaller data footprint
-- (SVGs can become huge!).
--
-- This implements the Ramer-Douglas-Peucker algorithm,
-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
simplifyPath :: Distance -> [Vec2] -> [Vec2]
simplifyPath epsilon  = V.toList . go . V.fromList
  where
    go :: Vector Vec2 -> Vector Vec2
    go points | V.length points <= 2 = points
    go points
      = let start = V.head points
            end = V.last points
            line = Line start end
            (dMax, iMax) = V.maximumBy (comparing fst) (V.imap (\i p -> (distanceFromLine p line, i)) points)
        in if dMax <= epsilon
            -- Points are all too close: remove them
            then V.fromList [start, end]
            -- Points far enough away: recurse
            else let before = V.take (iMax+1) points -- +1 so this includes the iMax point
                     after = V.drop iMax points
                 in go before <> V.drop 1 (go after) -- Don’t add the middle twice

pointOnBezier :: Bezier -> Double -> Vec2
pointOnBezier (Bezier a b c d) t
  =      (1-t)^3     *. a
    +. 3*(1-t)^2*t   *. b
    +. 3*(1-t)  *t^2 *. c
    +.           t^3 *. d

-- | Divide a Bezier into a number of points. One way to simplify a chain of Bezier
-- curves is by first subdividing them, then simplifying the resulting path, and
-- re-interpolating new Bezier curves through the data.
bezierSubdivide :: Int -> Bezier -> [Vec2]
bezierSubdivide n bezier = map (pointOnBezier bezier) ts
  where
    ts :: [Double]
    ts = map (\x -> fromIntegral x / fromIntegral (n-1)) [0..n-1]
