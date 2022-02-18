module Numerics.VectorAnalysis (

    -- * Operators with default settings
      grad
    , divergence
    , curl
    , curlZ
    , laplace

    -- * Operators with configurable step width
    , gradH
    , divH
    , curlH
    , curlZH
    , laplaceH
) where

import Geometry.Core

-- | A good standard value to use as a step size for taking derivatives.
standardH :: Double
standardH = 1e-3

-- | Gradient with predefined sampling distance.
--
-- \[
-- \text{grad}(f) = \begin{pmatrix}
--         \partial_x f \\
--         \partial_y f
--     \end{pmatrix}
-- \]
--
-- The fire-and-forget version of 'gradH'.
grad :: (Vec2 -> Double) -> Vec2 -> Vec2
grad = gradH standardH

-- | Divergence with predefined sampling distance. Named to avoid clashing with
-- integer 'div'ision.
--
-- \[
-- \text{div}(f) = \partial_xf_x + \partial_y f_y
-- \]
--
-- The fire-and-forget version of 'divH'.
divergence :: (Vec2 -> Vec2) -> Vec2 -> Double
divergence = divH standardH

-- | Curl with predefined sampling distance. Note that in two dimensions, the curl
-- is simply a number. For a different 2D adaptation of the \(\text{curl}\)
-- operator, see 'curlZ'.
--
-- \[
-- \text{curl}(f) = \partial_x f_y - \partial_y f_x
-- \]
--
-- The fire-and-forget version of 'curlH'.
curl :: (Vec2 -> Vec2) -> Vec2 -> Double
curl = curlH standardH

-- | Laplacian with predefined sampling distance.
--
-- \[
-- \nabla^2f = \partial^2_x f + \partial^2_y f
-- \]
--
-- The fire-and-forget version of 'laplaceH'.
laplace :: (Vec2 -> Double) -> Vec2 -> Double
laplace = laplaceH standardH

-- | Gradient with customizable sampling distance. The configurable version of 'grad'.
gradH
    :: Double -- ^ \(h\) as in \(\frac{f(x+h)-f(x)}h\)
    -> (Vec2 -> Double)
    -> (Vec2 -> Vec2)
gradH h f = \x ->
    let f_x = f x
    in Vec2 (f (x +. Vec2 h 0) - f_x) (f (x +. Vec2 0 h) - f_x) /. h

-- | Divergence with customizable sampling distance. The configurable version of 'divergence'.
divH
    :: Double -- ^ \(h\) as in \(\frac{f(x+h)-f(x)}h\)
    -> (Vec2 -> Vec2)
    -> (Vec2 -> Double)
divH h f = \v@(Vec2 x y) ->
    let f_v = f v
        Vec2 dx _ = f (Vec2 (x+h) y) -. f_v
        Vec2 _ dy = f (Vec2 x (y+h)) -. f_v
    in (dx+dy)/h

-- | Curl with customizable sampling distance. The configurable version of 'curl'.
curlH
    :: Double -- ^ \(h\) as in \(\frac{f(x+h)-f(x)}h\)
    -> (Vec2 -> Vec2)
    -> (Vec2 -> Double)
curlH h f = \v@(Vec2 x y) ->
    let f_v = f v
        Vec2 dy_fx _ = f (Vec2 x (y+h)) -. f_v
        Vec2 _ dx_fy = f (Vec2 (x+h) y) -. f_v
    in (dx_fy-dy_fx) / h

-- | Laplacian with customizable sampling distance. The configurable version of 'laplace'.
laplaceH
    :: Double -- ^ \(h\) as in \(\frac{f(x+h)-f(x)}h\)
    -> (Vec2 -> Double)
    -> (Vec2 -> Double)
laplaceH h = divH h . gradH h

-- | Curl of a purely-z-component 3D vector field, which is another common way
-- (than 'curl') to implement a two-dimensional version of \(\text{curl}\):
--
-- \[
-- \text{curl}_z(\psi)
--     = \left[\text{curl}_{3D} \begin{pmatrix}0\\0\\\psi\end{pmatrix}\right]_{\text{2D part}}
--     = \begin{pmatrix}\partial_y\psi\\-\partial_x\psi\end{pmatrix}
-- \]
--
-- Because of this, the result is always divergence-free, because \(\forall f. \text{div}(\text{curl}(f))=0\).
--
-- Using 'curlZ' to create 'divergence'-free flow fields is similar to using 'grad'
-- to create 'curl'-free force fields. The argument for 'curlZ' is known as the
-- vector or stream potential in fluid dynamics, with the resulting vector field
-- being known as a
-- [stream function](https://en.wikipedia.org/wiki/Stream_function).
--
-- The fire-and-forget version of 'curlZH'.
curlZ :: (Vec2 -> Double) -> Vec2 -> Vec2
curlZ f = curlZH standardH f

-- | Curl of the z component of a 3D vector field, with customizable sampling
-- distance. The configurable version of 'curlZ'.
curlZH
    :: Double -- ^ \(h\) as in \(\frac{f(x+h)-f(x)}h\)
    -> (Vec2 -> Double)
    -> Vec2
    -> Vec2
curlZH h f = \x ->
    let f_x = f x
    in Vec2 (f (x +. Vec2 0 h) - f_x) (- f (x +. Vec2 h 0) + f_x) /. h
        -- NB: this is just 90° rotated grad
