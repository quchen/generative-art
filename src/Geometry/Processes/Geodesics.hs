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
        (-c'x__V X X*x'^2 -2*c'x__V X Y*x'*y' -c'x__V Y Y*y'^2)
        (-c'y__V X X*x'^2 -2*c'y__V X Y*x'*y' -c'y__V Y Y*y'^2)
    )
  where
    h = 1e-3

    -- Offsets for derivatives at our current position
    vXH = v +. Vec2 h 0
    vYH = v +. Vec2 0 h

    -- First derivatives, applied to the offsets, for the second derivatives
    fdxvXH = (\p -> (f t (p +. Vec2 h 0) -. f t p) /. h) vXH
    fdxvYH = (\p -> (f t (p +. Vec2 h 0) -. f t p) /. h) vYH
    fdyvXH = (\p -> (f t (p +. Vec2 0 h) -. f t p) /. h) vXH
    fdyvYH = (\p -> (f t (p +. Vec2 0 h) -. f t p) /. h) vYH

    -- First derivatives applied at our current position
    fdxV = (f t vXH -. f t v) /. h
    fdyV = (f t vYH -. f t v) /. h

    -- Inverse metric g^{ab}
    (g'x'x, g'x'y, g'y'x, g'y'y) =
        let denominator = (1+fdxV^2+fdyV^2)
        in ( (1+fdyV^2)   /denominator
           , -(fdxV*fdyV) /denominator
           , g'x'y
           , (1+fdxV^2)   /denominator
        )

    -- Derivative of the metric g_{ab,c}
    g__d_V X X X = g_x_xd_xV
    g__d_V X X Y = g_x_xd_yV
    g__d_V X Y X = g_x_yd_xV
    g__d_V X Y Y = g_x_yd_yV
    g__d_V Y X X = g_y_xd_xV
    g__d_V Y X Y = g_y_xd_yV
    g__d_V Y Y X = g_y_yd_xV
    g__d_V Y Y Y = g_y_yd_yV

    -- Derivative of the metric g_{ab,c} at our current position
    g_x_xd_xV = ((1 + fdxvXH^2)    -. (1 + fdxV^2))  /. h
    g_x_xd_yV = ((1 + fdxvYH^2)    -. (1 + fdxV^2))  /. h
    g_x_yd_xV = ((fdxvXH * fdyvXH) -. (fdxV * fdyV)) /. h
    g_x_yd_yV = ((fdxvYH * fdyvYH) -. (fdxV * fdyV)) /. h
    g_y_xd_xV = ((fdxvXH * fdyvXH) -. (fdxV * fdyV)) /. h
    g_y_xd_yV = ((fdxvYH * fdyvYH) -. (fdxV * fdyV)) /. h
    g_y_yd_xV = ((1 + fdyvXH^2)    -. (1 + fdyV^2))  /. h
    g_y_yd_yV = ((1 + fdyvYH^2)    -. (1 + fdyV^2))  /. h

    -- Christoffel symbols, \Gamma^i_{kl} = \frac12 g^{im} (g_{mk,l}+g_{ml,k}-g_{kl,m})
    c'x__V k l = 0.5 * (g'x'x * (g__d_V X k l + g__d_V X l k - g__d_V k l X) + g'x'y * (g__d_V Y k l + g__d_V Y l k - g__d_V k l Y))
    c'y__V k l = 0.5 * (g'y'x * (g__d_V X k l + g__d_V X l k - g__d_V k l X) + g'y'y * (g__d_V Y k l + g__d_V Y l k - g__d_V k l Y))

-- | Spatial derivative
d :: VectorSpace v => Dim -> Double -> (Vec2 -> v) -> Vec2 -> v
d X h f v = (f (v +. Vec2 h 0) -. f v) /. h
d Y h f v = (f (v +. Vec2 0 h) -. f v) /. h

data Dim = X | Y
