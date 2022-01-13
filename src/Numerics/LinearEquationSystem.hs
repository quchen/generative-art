module Numerics.LinearEquationSystem (
    solveTridiagonal
) where

import           Algebra.VectorSpace
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V

-- | Solve a tridiagonal system of equations.
--
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
