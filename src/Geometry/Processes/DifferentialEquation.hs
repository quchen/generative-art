module Geometry.Processes.DifferentialEquation (
      rungeKuttaConstantStep
    , rungeKuttaAdaptiveStep
) where

import Geometry.Core

-- | Solve a system of first-order differential equations with RK4 AKA »the standard Runge-Kutta«.
rungeKutta4Step
    :: VectorSpace vec
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
    :: VectorSpace vec
    => (Double -> vec -> vec) -- ^ = dy/dt = f(t, y)
    -> vec                    -- ^ Initial y
    -> Double                 -- ^ Initial time
    -> Double                 -- ^ Step size
    -> [(Double, vec)]        -- ^ time, y
rungeKuttaConstantStep f y0 t0 dt
  = iterate (\(t, y) -> rungeKutta4Step f y t dt) (t0, y0)

-- | For the adaptive part of RKF, we need *some* norm to check how good or bad the
-- approximation is. This tolerance class implements the maximum norm for tuples,
-- and Euclidean norm for R^n.
class ToleranceNormedVector vec where
    toleranceNorm :: vec -> Double

instance ToleranceNormedVector Vec2 where
    toleranceNorm vec = let (Distance d) = norm vec in d

instance ToleranceNormedVector Double where
    toleranceNorm = abs

instance (ToleranceNormedVector a, ToleranceNormedVector b) => ToleranceNormedVector (a,b) where
    toleranceNorm (a,b) = max (toleranceNorm a) (toleranceNorm b)

-- | Solve a system of first-order differential equations with RKF45
-- (Runge-Kutta-Feinberg, adaptive step size using 4th-and-5th-order Runge-Kutta)
rkf45step
    :: (VectorSpace vec, ToleranceNormedVector vec)
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
        deviation = toleranceNorm (y'rk4 -. y'rk5)

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
    :: (VectorSpace vec, ToleranceNormedVector vec)
    => (Double -> vec -> vec)
    -> vec
    -> Double
    -> Double
    -> Double
    -> [(Double, vec)]
rungeKuttaAdaptiveStep f y0 t0 dt0 tolerance
  = [ (t, y) | (t,y,_dt) <- iterate (\(t, y, dt) -> rkf45step f y t dt tolerance) (t0, y0, dt0) ]
