module Numerics.VectorAnalysis (

    -- * Operators with default settings
      grad
    , divergence
    , curl
    , laplace

    -- * Operators with configurable step width
    , gradH
    , divH
    , curlH
    , laplaceH
) where

import Geometry.Core

-- | A good standard value to use as a step size for taking derivatives.
standardH :: Double
standardH = 1e-3

-- | Gradient with a predefined sampling distance.
--
-- \[
-- \begin{align}
-- \mathrm{grad} :\hspace{0.3em}
--     &\left(\mathbb R^2 \rightarrow \mathbb R\right)
--            \rightarrow \left(\mathbb R^2 \rightarrow \mathbb R^2\right) \\
--     &f \mapsto \begin{pmatrix}
--             \partial_x f \\
--             \partial_y f
--         \end{pmatrix}
-- \end{align}
-- \]
--
-- The fire-and-forget version of 'gradH'.
grad :: (Vec2 -> Double) -> Vec2 -> Vec2
grad = gradH standardH

-- | Divergence with a predefined sampling distance. Named to avoid clashing with
-- integer 'div'ision.
--
-- \[
-- \begin{align}
-- \mathrm{div} :\hspace{0.3em}
--     &\left(\mathbb R^2 \rightarrow \mathbb R^2\right)
--            \rightarrow \left(\mathbb R^2 \rightarrow \mathbb R\right) \\
--     &f \mapsto \partial_xf_x + \partial_y f_y
-- \end{align}
-- \]
--
-- The fire-and-forget version of 'divH'.
divergence :: (Vec2 -> Vec2) -> Vec2 -> Double
divergence = divH standardH

-- | Curl with a predefined sampling distance. Note that in two dimensions, the
-- curl is simply a number (or, more accurately, a 1-form).
--
-- \[
-- \begin{align}
-- \mathrm{curl} :\hspace{0.3em}
--     &\left(\mathbb R^2 \rightarrow \mathbb R^2\right)
--            \rightarrow \left(\mathbb R^2 \rightarrow \mathbb R\right) \\
--     &f \mapsto \partial_x f_y - \partial_y f_x
-- \end{align}
-- \]
--
-- The fire-and-forget version of 'curlH'.
curl :: (Vec2 -> Vec2) -> Vec2 -> Double
curl = curlH standardH

-- | Laplacian with a predefined sampling distance.
--
-- \[
-- \begin{align}
-- \mathrm{\nabla^2} :\hspace{0.3em}
--     &\left(\mathbb R^2 \rightarrow \mathbb R\right)
--            \rightarrow \left(\mathbb R^2 \rightarrow \mathbb R\right) \\
--     &f \mapsto \partial^2_x f + \partial^2_y f
-- \end{align}
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
-- Note that in two dimensions, the curl is simply a number (or, more accurately, a
-- 1-form).
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
