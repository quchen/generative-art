module Geometry.Bezier where

import Geometry.Core

-- | Cubic Bezier curve, defined by start, first/second control points, and end.
data Bezier = Bezier Vec2 Vec2 Vec2 Vec2 deriving (Eq, Ord, Show)

instance Transform Bezier where
    transform t (Bezier a b c d) = Bezier
        (transform t a)
        (transform t b)
        (transform t c)
        (transform t d)

instance HasBoundingBox Bezier where
    boundingBox bezier@(Bezier start _ _ end) = boundingBox ([start, end] ++ [bezierT bezier t | t <- extremalTs])
      where

        -- Alg idea: find the roots of the Bezier’s first derivative, collect
        -- Points where x/y components are extremal.
        -- Hastily written ugly code

        extremalTs = filter (\t -> t >=0 && t <= 1) (extremalT bezier)

        bezierT (Bezier a b c d) t
            =      (1-t)^3     *. a
              +. 3*(1-t)^2*t   *. b
              +. 3*(1-t)  *t^2 *. c
              +.           t^3 *. d
        extremalT (Bezier a b c d) = solveQuadratic (x a') (x b') (x c') ++ solveQuadratic (y a') (y b') (y c')
          where
            -- Coefficients of the first derivative polynomial of the Bezier curve
            a' = ((-3) *.a) +. (9*.b) -. 9*.c +. 3*.d
            b' = 6*.a -. 12*.b +. 6*.c
            c' = (-3)*.a +. 3*.b

            x (Vec2 x' _) = x'
            y (Vec2 _ y') = y'

        solveQuadratic a b c = case compare discriminant 0 of
            LT -> []
            EQ -> [-b/(2*a)]
            GT -> [ (-b + sqrt discriminant) / (2*a), (-b - sqrt discriminant) / (2*a)]
          where
            discriminant = b^2 - 4 * a * c
