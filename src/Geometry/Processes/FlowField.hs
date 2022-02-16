module Geometry.Processes.FlowField (
      fieldLine
    , flowLine
) where



import Geometry.Core



-- | Calculate a single field line of a vector field, i.e. a particle following its
-- arrows.
--
-- This can for example be used to create the typical magnetic field plots.
fieldLine
    :: (Vec2 -> Vec2) -- ^ Vector field
    -> time -- ^ (Unused) time parameter, kept so the result fits in the ODE solvers.
            --   Use 'flowLine' for time-dependent fields.
    -> Vec2 -- ^ Start of the flow line
    -> Vec2
fieldLine f = \_ x -> f x

-- | Calculate a single flow line of a vector field, i.e. the trajectory of a
-- particle following its arrows, by putting this into an ODE
-- solver such as 'Numerics.DifferentialEquation.rungeKuttaAdaptiveStep'.
--
-- This is a more general version of 'fieldLine', which also works for time-dependent fields.
flowLine
    :: (Double -> Vec2 -> Vec2) -- ^ Vector field
    -> Double -- ^ Time
    -> Vec2   -- ^ Start of the flow line
    -> Vec2
flowLine = id -- lol
