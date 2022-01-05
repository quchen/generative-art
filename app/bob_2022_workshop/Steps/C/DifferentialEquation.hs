module Steps.C.DifferentialEquation (
      rungeKuttaConstantStep
    , rungeKuttaAdaptiveStep
    , Vector(..)
    , NormedVector(..)
    , Vec2 (..)
    , Vec3 (..)
) where

-- | Solve a system of first-order differential equations with RK4 AKA »the standard Runge-Kutta«.
rungeKutta4Step
    :: Vector vec
    => (Double -> vec -> vec) -- ^ = dy/dt = f(t, y)
    -> vec                    -- ^ current y
    -> Double                 -- ^ Time
    -> Double                 -- ^ Step size
    -> (Double, vec)          -- ^ time, y
rungeKutta4Step f y t dt
  = let k1 = dt *. f t y
        k2 = dt *. f (t+0.5*dt) (y +. 0.5*.k1)
        k3 = dt *. f (t+0.5*dt) (y +. 0.5*.k2)
        k4 = dt *. f (t+dt) (y +. k3)

        dy = 1/6 *. (k1 +. 2*.k2 +. 2*.k3 +. k4)

    in (t + dt, y +. dy)

rungeKuttaConstantStep
    :: Vector vec
    => (Double -> vec -> vec) -- ^ = dy/dt = f(t, y)
    -> vec                           -- ^ Initial y
    -> Double                        -- ^ Initial time
    -> Double                        -- ^ Step size
    -> [(Double, vec)]               -- ^ time, y
rungeKuttaConstantStep f y0 t0 dt
  = iterate (\(t, y) -> rungeKutta4Step f y t dt) (t0, y0)

data Vec2 = Vec2 !Double !Double deriving (Eq, Ord, Show)
data Vec3 = Vec3 !Double !Double !Double deriving (Eq, Ord, Show)

class Vector vec where
    (+.) :: vec -> vec -> vec

    (-.) :: vec -> vec -> vec
    u -. v = u +. neg v

    neg :: vec -> vec
    neg v = (-1) *. v

    (*.) :: Double -> vec -> vec

    zero :: vec

class Vector vec => NormedVector vec where
    norm :: vec -> Double

infixl 6 +.
infixl 6 -.
infixl 7 *.

instance Vector Vec2 where
    Vec2 x1 y1 +. Vec2 x2 y2 = Vec2 (x1+x2) (y1+y2)
    a *. Vec2 x y = Vec2 (a*x) (a*y)
    neg (Vec2 x y) = Vec2 (-x) (-y)
    zero = Vec2 0 0

instance NormedVector Vec2 where
    norm (Vec2 x y) = sqrt (x^2 + y^2)

instance Vector Vec3 where
    Vec3 x1 y1 z1 +. Vec3 x2 y2 z2 = Vec3 (x1+x2) (y1+y2) (z1+z2)
    a *. Vec3 x y z = Vec3 (a*x) (a*y) (a*z)
    neg (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    zero = Vec3 0 0 0

instance NormedVector Vec3 where
    norm (Vec3 x y z) = sqrt (x^2 + y^2 + z^2)

instance (Vector a, Vector b) => Vector (a,b) where
    (a1, a2) +. (b1, b2) = (a1+.b1, a2+.b2)
    neg (a,b) = (neg a, neg b)
    x *. (a,b) = (x *. a, x *. b)
    zero = (zero, zero)

-- Not sure this instance is a very good idea, but I need it for the error
-- estimator in RKF45
instance (NormedVector a, NormedVector b) => NormedVector (a,b) where
    norm (a,b) = max (norm a) (norm b)

instance (Vector a, Vector b, Vector c) => Vector (a,b,c) where
    (a1, a2, a3) +. (b1, b2, b3) = (a1+.b1, a2+.b2, a3+.b3)
    neg (a,b,c) = (neg a, neg b, neg c)
    x *. (a,b,c) = (x *. a, x *. b, x *. c)
    zero = (zero, zero, zero)

-- | Solve a system of first-order differential equations with RKF45
-- (Runge-Kutta-Feinberg, adaptive step size using 4th-and-5th-order Runge-Kutta)
rkf45step
    :: NormedVector vec
    => (Double -> vec -> vec) -- ^ = dy/dt = f(t, y)
    -> vec                    -- ^ current y
    -> Double                 -- ^ current time
    -> Double                 -- ^ step size
    -> Double                 -- ^ Error tolerance
    -> (Double, vec, Double)  -- ^ new time, new y, new step size
rkf45step f y t dt tolerance
  = let k1 = dt *. f t y
        k2 = dt *. f (t + 1/4*dt)   (y +. (1/4)       *.k1)
        k3 = dt *. f (t + 3/8*dt)   (y +. (3/32)      *.k1 +. (9/32)     *.k2)
        k4 = dt *. f (t + 12/13*dt) (y +. (1932/2197) *.k1 -. (7200/2197)*.k2 +. (7296/2197)*.k3)
        k5 = dt *. f (t + dt)       (y +. (439/216)   *.k1 -. 8          *.k2 +. (3680/513) *.k3 -. (845/4104) *.k4)
        k6 = dt *. f (t + 1/2*dt)   (y -. (8/27)      *.k1 +. 2          *.k2 -. (3544/2565)*.k3 +. (1859/4104)*.k4 -. (11/40)*.k5)

        y'rk4 = y +. (25/216)*.k1 +. 0*.k2 +. (1408/2565) *.k3 +. (2197/4101)  *.k4 -. (1/5) *.k5
        y'rk5 = y +. (16/135)*.k1 +. 0*.k2 +. (6656/12826)*.k3 +. (28561/56430)*.k4 -. (9/50)*.k5 +. (2/55)*.k6

        -- How far are we from the tolerance threshold?
        deviation = norm (y'rk4 -. y'rk5)

        -- Safety constant. Mathematically unnecessary, but some authors seem to
        -- use it to err on the safer side when it comes to reducing step size
        -- to avoid flip-flopping.
        safety = 0.9

    in if deviation < tolerance
        then let dtFactor = min 2 $ safety * (tolerance / deviation) ** (1/4)
                 dt' = dt * dtFactor
             in (t+dt, y'rk5, dt')
        else let dtFactor = max 0.1 $ safety * (tolerance / deviation) ** (1/5)
                 dt' = dt * dtFactor
             in rkf45step f y t dt' tolerance

rungeKuttaAdaptiveStep
    :: NormedVector vec
    => (Double -> vec -> vec)
    -> vec
    -> Double
    -> Double
    -> Double
    -> [(Double, vec)]
rungeKuttaAdaptiveStep f y0 t0 dt0 tolerance
  = [ (t, y) | (t,y,_dt) <- iterate (\(t, y, dt) -> rkf45step f y t dt tolerance) (t0, y0, dt0) ]
