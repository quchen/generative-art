{-# LANGUAGE PatternSynonyms #-}
module AccVectorSpace where

import Prelude ()
import Data.Array.Accelerate

-- | A generic vector space. Not only classic vectors like 'Vec2' form a vector
-- space, but also concepts like 'Angle's – anything that can be added, inverted,
-- and multiplied with a scalar.
--
-- Vector spaces come with a number of laws that you can look up on Wikipedia. The
-- short version is: each operation behaves like addition/multiplication on normal
-- numbers.
class AccVectorSpace v where
    {-# MINIMAL (+..), (*..), ((-..) | anegateV), azero #-}
    -- | Vector addition
    (+..) :: Exp v -> Exp v -> Exp v

    -- | Vector subtraction
    (-..) :: Exp v -> Exp v -> Exp v
    a -.. b = a +.. anegateV b

    -- | Multiplication with a scalar
    (*..) :: Exp Double -> Exp v -> Exp v

    -- | Division by a scalar
    (/..) :: Exp v -> Exp Double -> Exp v
    v /.. a = (1/a) *.. v

    -- | Neutral element
    azero :: Exp v

    -- | Inverse element
    anegateV :: Exp v -> Exp v
    anegateV a = (-1) *.. a

infixl 6 +.., -..
infixl 7 *.., /..

instance (Elt v1, Elt v2, AccVectorSpace v1, AccVectorSpace v2) => AccVectorSpace (v1, v2) where
    T2 u1 v1 +.. T2 u2 v2 = lift (u1+..u2, v1+..v2)
    T2 u1 v1 -.. T2 u2 v2 = lift (u1-..u2, v1-..v2)
    a *.. T2 u1 v1 = lift (a*..u1, a*..v1)
    azero = lift (azero, azero)

instance (Elt v1, Elt v2, Elt v3, AccVectorSpace v1, AccVectorSpace v2, AccVectorSpace v3) => AccVectorSpace (v1, v2, v3) where
    T3 u1 v1 w1 +.. T3 u2 v2 w2 = lift (u1+..u2, v1+..v2, w1+..w2)
    T3 u1 v1 w1 -.. T3 u2 v2 w2 = lift (u1-..u2, v1-..v2, w1-..w2)
    a *.. T3 u1 v1 w1 = lift (a*..u1, a*..v1, a*..w1)
    azero = lift (azero, azero, azero)

instance (Elt v1, Elt v2, Elt v3, Elt v4, AccVectorSpace v1, AccVectorSpace v2, AccVectorSpace v3, AccVectorSpace v4) => AccVectorSpace (v1, v2, v3, v4) where
    T4 u1 v1 w1 x1 +.. T4 u2 v2 w2 x2 = lift (u1+..u2, v1+..v2, w1+..w2, x1+..x2)
    T4 u1 v1 w1 x1 -.. T4 u2 v2 w2 x2 = lift (u1-..u2, v1-..v2, w1-..w2, x1-..x2)
    a *.. T4 u1 v1 w1 x1 = lift (a*..u1, a*..v1, a*..w1, a*..x1)
    azero = lift (azero, azero, azero, azero)

instance AccVectorSpace Double where
    a +.. b = a+b
    a *.. b = a*b
    a -.. b = a-b
    azero = 0
