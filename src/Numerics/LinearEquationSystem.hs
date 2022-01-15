module Numerics.LinearEquationSystem (
    solveTridiagonal
) where

import           Algebra.VectorSpace
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V

-- | Solve a tridiagonal system of equations.
--
-- \[
-- \begin{pmatrix}
--     b_0 & c_0 &        &         &         \\
--     a_0 & b_1 & c_1    &         &         \\
--         & a_1 & b_2    & \ddots  &         \\
--         &     & \ddots & \ddots  & c_{n-2} \\
--         &     &        & a_{n-2} & b_{n-1}
-- \end{pmatrix}
-- \begin{pmatrix}
--     x_0     \\
--     x_1     \\
--     x_2     \\
--     \vdots  \\
--     x_{n-1}
-- \end{pmatrix}
-- =
-- \begin{pmatrix}
--     d_0     \\
--     d_1     \\
--     d_2     \\
--     \vdots  \\
--     d_{n-1}
-- \end{pmatrix}
-- \]
--
-- Translated with blood, sweat and tears from 1-and-2(!!)-based indexing
-- at https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
solveTridiagonal
    :: VectorSpace vec
    => Vector Double -- ^ Lower diagonal, length n-1. \(a_0 \ldots a_{n-2}\)
    -> Vector Double -- ^ Diagonal, length n. \(b_0 \ldots b_{n-1}\)
    -> Vector Double -- ^ Upper diagonal, length n-1. \(c_0 \ldots c_{n-2}\)
    -> Vector vec    -- ^ RHS, length n. \(d_0 \ldots d_{n-1}\)
    -> Vector vec    -- ^ Solution, length n. \(x_0 \ldots x_{n-1}\)
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
