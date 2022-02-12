{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Physics where

import Control.Applicative (liftA2)

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

newtype NBody a = NBody { getNBody :: [a] } deriving (Functor, Foldable)

instance Applicative NBody where
    pure a = NBody (repeat a)
    liftA2 f (NBody xs) (NBody ys) = NBody (zipWith f xs ys)

instance Traversable NBody where
    traverse f (NBody xs) = NBody <$> traverse f xs

instance VectorSpace a => VectorSpace (NBody a) where
    (+.) = liftA2 (+.)
    a *. xs = fmap (a *.) xs
    (-.) = liftA2 (-.)
    zero = NBody (repeat zero)

nBody :: (Vec2 -> Vec2 -> Double) -> NBody Double -> NBody PhaseSpace -> NBody PhaseSpace
nBody interaction masses particles = NBody
    [ PhaseSpace deltaP deltaQ
    | (i, m1, PhaseSpace p1 q1) <- zip3 [0..] (getNBody masses) (getNBody particles)
    , let deltaQ = p1 /. m1
    , let deltaP = vsum
            [ negateV (grad (interaction q2) q1)
            | (j, PhaseSpace _ q2) <- zip [0..] (getNBody particles)
            , i /= j
            ]
    ]
