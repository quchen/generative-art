{-# LANGUAGE BangPatterns #-}

-- https://www.michael-joost.de/bezierfit.pdf
-- https://www.stkent.com/2015/07/03/building-smooth-paths-using-bezier-curves.html
module Geometry.BezierInterpolation.Internal where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Vector (Vector, (!))
import Data.Vector.Mutable (STVector)
import Control.Monad.ST
import Control.Applicative
import Data.Traversable
import Graphics.Rendering.Cairo hiding (x,y)
import Data.Foldable
import Debug.Trace

data Vec2 = Vec2 !Double !Double deriving (Eq, Ord, Show)
(+.), (-.) :: Vec2 -> Vec2 -> Vec2
Vec2 x1 y1 +. Vec2 x2 y2 = Vec2 (x1+x2) (y1+y2)
u -. v = u +. (-1) *. v
(*.) :: Double -> Vec2 -> Vec2
a *. Vec2 x y = Vec2 (a*x) (a*y)
(/.) :: Vec2 -> Double -> Vec2
u /. a = (1/a) *. u
infixl 6 +.
infixl 6 -.
infixl 7 *.
infixl 7 /.

-- | Cubic Bezier curve
data Bezier = Bezier Vec2 Vec2 Vec2 Vec2

-- | Smoothen a number of points by putting a Bezier curve between each pair.
-- This function is the open version, so it will not close the curve smoothly.
bezierSmoothenOpen :: [Vec2] -> [Bezier]
bezierSmoothenOpen points = V.toList (V.izipWith f pointsV (V.tail pointsV))
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
    f i start end
      = Bezier start
               (controlPointsStart ! i)
               (controlPointsEnd ! i)
               end

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

target :: Int -> Vector Vec2 -> Vector Vec2
target n vertices = V.generate n $ \i -> case () of
    _ | i == 0    ->      vertices ! 0     +. 2 *. vertices ! 1
      | i == n-1  -> 8 *. vertices ! (n-1) +.      vertices ! n
      | otherwise -> 4 *. vertices ! i     +. 2 *. vertices ! (i+1)

-- See https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
-- Translated with blood, sweat and tears from 1-and-2(!!)-based indexing
solveTridiagonal
    :: Vector Double -- ^ Lower diagonal, length n-1
    -> Vector Double -- ^ Diagonal, length n
    -> Vector Double -- ^ Upper diagonal, length n-1
    -> Vector Vec2   -- ^ RHS, length n
    -> Vector Vec2
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
