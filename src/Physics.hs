module Physics where

import Geometry
import Numerics.VectorAnalysis

data PhaseSpace = PhaseSpace
    { p :: Vec2
    , q :: Vec2
    }

instance VectorSpace PhaseSpace where
    zero = PhaseSpace zero zero
    PhaseSpace p1 q1 +. PhaseSpace p2 q2 = PhaseSpace (p1 +. p2) (q1 +. q2)
    c *. PhaseSpace p q = PhaseSpace (c *. p) (c *. q)
    negateV (PhaseSpace p q) = PhaseSpace (negateV p) (negateV q)

particleInPotential :: Double -> (Vec2 -> Double) -> PhaseSpace -> PhaseSpace
particleInPotential mass potential PhaseSpace{..} = PhaseSpace deltaP deltaQ
  where
    deltaP = negateV (grad potential q)
    deltaQ = p /. mass

--particleInPotentialWithFriction :: Double -> (Vec2 -> Double) -> Double -> PhaseSpace -> PhaseSpace
--particleInPotentialWithFriction mass potential friction = PhaseSpace deltaP deltaQ
--  where
--    deltaP = negateV (grad potential q)
--    deltaQ = p /. mass


harmonicPotential :: (Double, Double) -> Vec2 -> Vec2 -> Double
harmonicPotential (w, h) (Vec2 x0 y0) (Vec2 x y) = ((x-x0)/w)^2 + ((y-y0)/h)^2

coulombPotential :: Double -> Vec2 -> Vec2 -> Double
coulombPotential charge center particle = charge / norm (particle -. center)

twoBody :: (Vec2 -> Vec2 -> Double) -> (Double, Double) -> (PhaseSpace, PhaseSpace) -> (PhaseSpace, PhaseSpace)
twoBody interaction (m1, m2) (PhaseSpace p1 q1, PhaseSpace p2 q2) = (PhaseSpace deltaP1 deltaQ1, PhaseSpace deltaP2 deltaQ2)
  where
    deltaP1 = negateV (grad (\q -> interaction q q2) q1)
    deltaP2 = negateV (grad (\q -> interaction q1 q) q2)
    deltaQ1 = p1 /. m1
    deltaQ2 = p2 /. m2

--nBody :: (Vec2 -> Vec2 -> Double) -> [Double] -> [PhaseSpace] -> [PhaseSpace]
