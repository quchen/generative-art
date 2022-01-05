module Steps.C.DifferentialEquation (
    rungeKutta4Solve
    , Vector(..)
    , NormedVector(..)
    , Vec2 (..)
    , Vec3 (..)
) where

-- | Solve a second-order differential equation with Runge-Kutta.
rungeKutta4Step
    :: Vector vec
    => (vec -> vec -> Double -> vec) -- ^ dx/dt
    -> (vec -> vec -> Double -> vec) -- ^ dv/dt
    -> vec                           -- ^ Position
    -> vec                           -- ^ Velocity
    -> Double                        -- ^ Time
    -> Double                        -- ^ Step size
    -> (vec, vec, Double)            -- ^ Position, velocity, time
rungeKutta4Step dxdt dvdt x v t dt
  = let k11 = dt *. dxdt x v t
        k21 = dt *. dvdt x v t

        k12 = dt *. dxdt (x +. 0.5*.k11) (v +. 0.5*.k21) (t+0.5*dt)
        k22 = dt *. dvdt (x +. 0.5*.k11) (v +. 0.5*.k21) (t+0.5*dt)

        k13 = dt *. dxdt (x +. 0.5*.k12) (v +. 0.5*.k22) (t+0.5*dt)
        k23 = dt *. dvdt (x +. 0.5*.k12) (v +. 0.5*.k22) (t+0.5*dt)

        k14 = dt *. dxdt (x +. k13) (v +. k23) (t+dt)
        k24 = dt *. dvdt (x +. k13) (v +. k23) (t+dt)

        dx = 1/6 *. (k11 +. 2*.k12 +. 2*.k13 +. k14)
        dv = 1/6 *. (k21 +. 2*.k22 +. 2*.k23 +. k24)

    in (x +. dx, v +. dv, t + dt)

rungeKutta4Solve
    :: Vector t
    => (t -> t -> Double -> t) -- ^ dx/dt
    -> (t -> t -> Double -> t) -- ^ dv/dt
    -> t                       -- ^ Initial position
    -> t                       -- ^ Initial velocity
    -> Double                  -- ^ Initial time
    -> Double                  -- ^ Step size
    -> [(t, t, Double)]        -- ^ Position, velocity, time
rungeKutta4Solve dxdt dvdt x0 v0 t0 dt
  = iterate (\(x, v, t) -> rungeKutta4Step dxdt dvdt x v t dt) (x0, v0, t0)

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

instance (Vector a, Vector b, Vector c) => Vector (a,b,c) where
    (a1, a2, a3) +. (b1, b2, b3) = (a1+.b1, a2+.b2, a3+.b3)
    neg (a,b,c) = (neg a, neg b, neg c)
    x *. (a,b,c) = (x *. a, x *. b, x *. c)
    zero = (zero, zero, zero)

rkf45step
    :: NormedVector vec
    => (Double -> vec -> vec) -- ^ = dy/dt = f(t, y)
    -> vec                    -- ^ current y
    -> Double                 -- ^ current time
    -> Double                 -- ^ step size
    -> Double                 -- ^ Error tolerance
    -> (vec, Double, Double)  -- ^ new y, new time, new step size
rkf45step f y t dt epsilon
  = let k1 = dt *. f t y
        k2 = dt *. f (t + 1/4*dt)   (y +. (1/4)       *.k1)
        k3 = dt *. f (t + 3/8*dt)   (y +. (3/32)      *.k1 +. (9/32)     *.k2)
        k4 = dt *. f (t + 12/13*dt) (y +. (1932/2197) *.k1 -. (7200/2197)*.k2 +. (7296/2197)*.k3)
        k5 = dt *. f (t + dt)       (y +. (439/216)   *.k1 -. 8          *.k2 +. (3680/513) *.k3 -. (845/4104) *.k4)
        k6 = dt *. f (t + 1/2*dt)   (y -. (8/27)      *.k1 +. 2          *.k2 -. (3544/2565)*.k3 +. (1859/4104)*.k4 -. (11/40)*.k5)

        y'rk4 = y +. (25/216)*.k1 +. 0*.k2 +. (1408/2565) *.k3 +. (2197/4101)  *.k4 -. (1/5) *.k5
        y'rk5 = y +. (16/135)*.k1 +. 0*.k2 +. (6656/12826)*.k3 +. (28561/56430)*.k4 -. (9/50)*.k5 +. (2/55)*.k6
        dtIdeal = dt * safety * (epsilon / norm (y'rk4 -. y'rk5)) ** (1/5)

        -- Safety constant. Mathematically unnecessary, but some authors seem to
        -- use it to err on the safer side when it comes to reducing step size.
        safety = 0.9

    in if dtIdeal < dt
        then rkf45step f y t dtIdeal epsilon -- Repeat with finer step size
        else (y'rk5, t+dt, dtIdeal) -- Accept with increased step size
