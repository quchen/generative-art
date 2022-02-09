module Geometry.Processes.Geodesics (geodesicEquation) where



import Geometry.Core



-- | The geodesic is the shortest path between two points.
--
-- This function allows creating the geodesic differential equation, suitable for
-- using in ODE solvers such as
-- 'Numerics.DifferentialEquation.rungeKuttaAdaptiveStep'.
--
-- The equation is very simple, as long as you don’t have to implement it.
--
-- \[
-- \ddot v^i = \Gamma^i_{kl}\dot v^k\dot v^l \\
-- \Gamma^i_{kl} = \frac12 g^{im} (g_{mk,l}+g_{ml,k}-g_{kl,m}) \\
-- g_{ij}(f) = \left\langle \partial_if,\partial_jf \right\rangle
-- \]
--
-- Go ahead, look at the code, I dare you
geodesicEquation
    :: (Double -> Vec2 -> Double) -- ^ Surface function \(f(t, \mathbf v)\)
    -> Double                     -- ^ Time \(t\)
    -> (Vec2, Vec2)               -- ^ \((\mathbf v, \dot{\mathbf v})\)
    -> (Vec2, Vec2)               -- ^ \((\dot{\mathbf v}, \ddot{\mathbf v})\)
geodesicEquation f t (v, v'@(Vec2 x' y')) =
    ( v'
    , Vec2
        (-c'__ X X X v*x'^2 -2*c'__ X X Y v*x'*y' -c'__ X Y Y v*y'^2)
        (-c'__ Y X X v*x'^2 -2*c'__ Y X Y v*x'*y' -c'__ Y Y Y v*y'^2)
    )
  where
    h = 1e-3

    f_dx = d X h (f t)
    f_dy = d Y h (f t)

    f_dxV = f_dx v
    f_dyV = f_dy v

    -- Metric g_{ab}
    g__ X X p = 1 + f_dx p^2
    g__ X Y p = f_dx p * f_dy p
    g__ Y X p = g__ X Y p
    g__ Y Y p = 1 + f_dy p^2

    -- Inverse metric g^{ab}
    denomV = (1+f_dxV^2+f_dyV^2)
    g'' X X = (1+f_dyV^2)    /denomV
    g'' X Y = -(f_dxV*f_dyV) /denomV
    g'' Y X = g'' X Y
    g'' Y Y = (1+f_dxV^2)    /denomV

    -- Derivative of the metric g_{ab,c}
    g__d_ a b c = d c h (g__ a b)

    -- Christoffel symbols, \Gamma^i_{kl} = \frac12 g^{im} (g_{mk,l}+g_{ml,k}-g_{kl,m})
    c'__ i k l p = 0.5 * sum [ g'' i m * (g__d_ m k l p + g__d_ m l k p - g__d_ k l m p) | m <- [X,Y]]

-- | Spatial derivative
d :: VectorSpace v => Dim -> Double -> (Vec2 -> v) -> Vec2 -> v
d X h f v = (f (v +. Vec2 h 0) -. f v) /. h
d Y h f v = (f (v +. Vec2 0 h) -. f v) /. h

data Dim = X | Y
