module Algebra.VectorSpace where

-- | A generic vector space. Not only classic vectors like 'Vec2' form a vector
-- space, but also concepts like 'Angle's or 'Distance's – anything that can be
-- added, inverted, and multiplied with a scalar.
--
-- Vector space laws:
--
--     (1) Associativity of addition: @a +. (b +. c) = (a +. b) +. c@
--     (2) Neutral ('zero'): @a +. 'zero' = a = 'zero' +. a@
--     (3) Inverse ('negateV'): @a +. 'negateV' a = 'zero' = 'negateV' a +. a@. '(-.)' is a shorthand for the inverse: @a -. b = a +. negate b@.
--     (4) Commutativity of addition: @a +. b = b +. a@
--     (5) Distributivity of scalar multiplication 1: @a *. (b +. c) = a *. b +. a *. c@
--     (6) Distributivity of scalar multiplication 2: @(a + b) *. c = a *. c +. b *. c@
--     (7) Compatibility of scalar multiplication: @(a * b) *. c = a *. (b *. c)@
--     (8) Scalar identity: @1 *. a = a@
class VectorSpace v where
    {-# MINIMAL (+.), (*.), ((-.) | negateV), zero #-}
    -- | Vector addition
    (+.) :: v -> v -> v

    -- | Vector subtraction
    (-.) :: v -> v -> v
    a -. b = a +. negateV b

    -- | Multiplication with a scalar
    (*.) :: Double -> v -> v

    -- | Division by a scalar
    (/.) :: v -> Double -> v
    v /. a = (1/a) *. v

    -- | Neutral element
    zero :: v

    -- | Inverse element
    negateV :: v -> v
    negateV a = (-1) *. a

infixl 6 +., -.
infixl 7 *., /.

instance (VectorSpace v1, VectorSpace v2) => VectorSpace (v1, v2) where
    (u1, v1) +. (u2, v2) = (u1+.u2, v1+.v2)
    (u1, v1) -. (u2, v2) = (u1-.u2, v1-.v2)
    a *. (u1, v1) = (a*.u1, a*.v1)
    zero = (zero, zero)

instance (VectorSpace v1, VectorSpace v2, VectorSpace v3) => VectorSpace (v1, v2, v3) where
    (u1, v1, w1) +. (u2, v2, w2) = (u1+.u2, v1+.v2, w1+.w2)
    (u1, v1, w1) -. (u2, v2, w2) = (u1-.u2, v1-.v2, w1-.w2)
    a *. (u1, v1, w1) = (a*.u1, a*.v1, a*.w1)
    zero = (zero, zero, zero)

instance (VectorSpace v1, VectorSpace v2, VectorSpace v3, VectorSpace v4) => VectorSpace (v1, v2, v3, v4) where
    (u1, v1, w1, x1) +. (u2, v2, w2, x2) = (u1+.u2, v1+.v2, w1+.w2, x1+.x2)
    (u1, v1, w1, x1) -. (u2, v2, w2, x2) = (u1-.u2, v1-.v2, w1-.w2, x1-.x2)
    a *. (u1, v1, w1, x1) = (a*.u1, a*.v1, a*.w1, a*.x1)
    zero = (zero, zero, zero, zero)

instance (VectorSpace v1, VectorSpace v2, VectorSpace v3, VectorSpace v4, VectorSpace v5) => VectorSpace (v1, v2, v3, v4, v5) where
    (u1, v1, w1, x1, y1) +. (u2, v2, w2, x2, y2) = (u1+.u2, v1+.v2, w1+.w2, x1+.x2, y1+.y2)
    (u1, v1, w1, x1, y1) -. (u2, v2, w2, x2, y2) = (u1-.u2, v1-.v2, w1-.w2, x1-.x2, y1-.y2)
    a *. (u1, v1, w1, x1, y1) = (a*.u1, a*.v1, a*.w1, a*.x1, a*.y1)
    zero = (zero, zero, zero, zero, zero)

instance VectorSpace Double where
    a +. b = a+b
    a *. b = a*b
    a -. b = a-b
    zero = 0

instance VectorSpace b => VectorSpace (a -> b) where
    (f +. g) a = f a +. g a
    (c *. f) a = c *. f a
    (f -. g) a = f a -. g a
    zero = const zero
