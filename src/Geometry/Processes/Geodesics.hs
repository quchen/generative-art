module Geometry.Processes.Geodesics (geodesicEquation) where



import Geometry.Core



-- | Geodesic equation of a two-dimensional function, suitable for using in ODE
-- solvers such as
-- 'Numerics.DifferentialEquation.rungeKuttaAdaptiveStep'.
--
-- https://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node195.html
--
-- https://github.com/rndsrc/bhrad/blob/main/5.geodesic.ipynb
--
-- \[
-- \ddot v^a = \Gamma^a_{bc}\dot v^b\dot v^c
-- \]
geodesicEquation
    :: (Double -> Vec2 -> Double) -- ^ Surface function \(f(t, \mathbf v)\)
    -> Double                     -- ^ Time \(t\)
    -> (Vec2, Vec2)               -- ^ \((\mathbf v, \dot{\mathbf v})\)
    -> (Vec2, Vec2)               -- ^ \((\dot{\mathbf v}, \ddot{\mathbf v})\)
geodesicEquation f t (v, v'@(Vec2 x' y')) =
    ( v'
    , Vec2
        (-c'x_xx v*x'^2 -2*c'x_xy v*x'*y' -c'x_yy v*y'^2)
        (-c'y_xx v*x'^2 -2*c'y_xy v*x'*y' -c'y_yy v*y'^2)
    )
  where
    h = 1e-3

    f_x, f_y, f_xx, f_xy, f_yx, f_yy :: Vec2 -> Double
    f_x  = d X h (f t)
    f_y  = d Y h (f t)
    f_xx = d X h f_x
    f_xy = d X h f_y
    f_yx = f_xy -- this is one of the few non-awful parts in this code
    f_yy = d Y h f_y

    -- Metric
    g_xx p = 1 + f_x p^2
    g_xy p = f_x p * f_y p
    g_yx p = g_xy p
    g_yy p = 1 + f_y p^2

    -- Inverse metric
    denom p = (1+f_x p^2+f_y p^2)
    g'xx p = (1+f_y p^2)    /denom p
    g'xy p = -(f_x p*f_y p) /denom p
    g'yx p = g'xy p
    g'yy p = (1+f_x p^2)    /denom p

    -- Derivative of the metric
    g_xx_x = d X h g_xx
    g_xy_x = d X h g_xy
    g_yx_x = g_yx_x
    g_yy_x = d X h g_yy
    g_xx_y = d Y h g_xx
    g_xy_y = d Y h g_xy
    g_yx_y = g_xy_y
    g_yy_y = d Y h g_yy

    -- Christoffel symbols, \Gamma^i_{jk} = \frac12 g^{im} (g_{mk,l}+g_{ml,k}-g_{kl,m})
    c'x_xx p = 0.5 * ( g'xx p * (g_xx_x p + g_xx_x p - g_xx_x p) + g'xy p * (g_yx_x p + g_yx_x p - g_xx_y p) )
    c'x_xy p = 0.5 * ( g'xx p * (g_xx_y p + g_xy_x p - g_xy_x p) + g'xy p * (g_yx_y p + g_yy_x p - g_xy_y p) )
    c'x_yy p = 0.5 * ( g'xx p * (g_xy_y p + g_xy_y p - g_yy_x p) + g'xy p * (g_yy_y p + g_yy_y p - g_yy_y p) )
    c'y_xx p = 0.5 * ( g'yx p * (g_xx_x p + g_xx_x p - g_xx_x p) + g'yy p * (g_yx_x p + g_yx_x p - g_xx_y p) )
    c'y_xy p = 0.5 * ( g'yx p * (g_xx_y p + g_xy_x p - g_xy_x p) + g'yy p * (g_yx_y p + g_yy_x p - g_xy_y p) )
    c'y_yy p = 0.5 * ( g'yx p * (g_xy_y p + g_xy_y p - g_yy_x p) + g'yy p * (g_yy_y p + g_yy_y p - g_yy_y p) )

-- | Spatial derivative
d :: VectorSpace v => Dim -> Double -> (Vec2 -> v) -> Vec2 -> v
d X h f v = (f (v +. Vec2 h 0) -. f v) /. h
d Y h f v = (f (v +. Vec2 0 h) -. f v) /. h

data Dim = X | Y
