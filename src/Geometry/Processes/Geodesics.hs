module Geometry.Processes.Geodesics (geodesicEquation) where



import Geometry.Core



-- | Geodesic equation of a two-dimensional function, suitable for using in ODE
-- solvers such as
-- 'Numerics.DifferentialEquation.rungeKuttaAdaptiveStep'.
--
-- https://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node195.html
--
-- \[
-- \ddot v^a = \Gamma^a_{bc}\dot v^b\dot v^c
-- \]
geodesicEquation
    :: (Double -> Vec2 -> Double) -- ^ Surface function \(f(t, \mathbf v)\)
    -> Double                     -- ^ Time \(t\)
    -> (Vec2, Vec2)               -- ^ \((\mathbf v, \dot{\mathbf v})\)
    -> (Vec2, Vec2)
geodesicEquation f t (v@(Vec2 x y), Vec2 x' y') =
    ( Vec2 x' y'
    , Vec2
        (- (c111)*x'^2 - 2 * (c112)*x'*y' - (c122)*y'^2)
        (- (c211)*x'^2 - 2 * (c212)*x'*y' - (c222)*y'^2)
    )
  where
    h = 1e-3
    f_x = (f t (v +. Vec2 h 0) -. f t v) /. h
    f_y = (f t (v +. Vec2 0 h) -. f t v) /. h

    g = ( 1+f_x^2, f_x*f_y
        , f_x*f_y, 1+f_x^2 )

    -- Christoffel symbols, \Gamma^i_{jk}
    c111 = undefined
    c112 = undefined
    c122 = undefined
    c211 = undefined
    c212 = undefined
    c222 = undefined
