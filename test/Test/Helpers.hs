{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Helpers (
      Seed(..)
    , gaussianVecs
    , GaussianVec(..)
    , LotsOfGaussianPoints(..)

    , EqApprox(..)
    , Tolerance(..)
    , (~==)
) where



import Data.List
import System.Random

import Geometry

import Test.Tasty.QuickCheck



instance Arbitrary Vec2 where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary
    shrink (Vec2 x y) = [ Vec2 x' y' | x' <- [0,1,-1], y' <- [0,1,-1] ]
                     ++ [ Vec2 x' y' | (x', y') <- shrink (x, y) ]

instance Arbitrary Angle where
    arbitrary = fmap deg (choose (0, 360))

newtype GaussianVec = GaussianVec Vec2
    deriving (Eq, Ord, Show)

instance Arbitrary GaussianVec where
    arbitrary = do
        u1 <- choose (0,1)
        if u1 == 0 -- to avoid diverging on log(0)
            then arbitrary
            else do
                u2 <- choose (0, 2*pi)
                let root1 = sqrt (-2 * log u1)
                    x = root1 * cos u2
                    y = root1 * sin u2
                pure (GaussianVec (Vec2 x y))
    shrink (GaussianVec vec) = map GaussianVec (shrink vec)

newtype LotsOfGaussianPoints = LotsOfGaussianPoints [Vec2]
    deriving (Eq, Ord, Show)

instance Arbitrary LotsOfGaussianPoints where
    arbitrary = do
        seed <- arbitrary
        numPoints <- choose (10, 100)
        (pure . LotsOfGaussianPoints . take numPoints . gaussianVecs) seed
    shrink (LotsOfGaussianPoints xs)
      = let -- Some sufficiently chaotic seed function :-)
            seed = foldl' (\(Seed s) vec -> Seed (round (normSquare vec) + s*s)) (Seed 0) xs
        in map (\numPoints -> LotsOfGaussianPoints (take numPoints (gaussianVecs seed)))
               [3 .. length xs-1]

newtype Seed = Seed Int
    deriving (Eq, Ord, Show)

instance Arbitrary Seed where
    arbitrary = fmap (\(Large s) -> Seed s) arbitrary

gaussianVecs :: Seed -> [Vec2]
gaussianVecs (Seed seed)
  = let go (u1:rest)
            | u1 <= 0 = go rest -- to avoid diverging on log(0)
        go (u1:u2:rest)
            = let root1 = sqrt (-2 * log u1)
                  pi2u2 = 2 * pi * u2
                  x = root1 * cos pi2u2
                  y = root1 * sin pi2u2
              in Vec2 x y : go rest
        go _ = error "Can’t happen, input is infinite"
    in go (randomRs (0, 1) (mkStdGen seed))
newtype Tolerance = Tolerance Double

class EqApprox a where
    approxEqualTolerance :: Tolerance -> a -> a -> Bool

infix 4 ~==
(~==) :: EqApprox a => a -> a -> Bool
(~==) = approxEqualTolerance (Tolerance 1e-10)

instance EqApprox Double where
    approxEqualTolerance (Tolerance tolerance) reference value
      = abs (reference - value) <= tolerance

instance EqApprox Vec2 where
    approxEqualTolerance (Tolerance tolerance) v1 v2
      = let Distance distance = norm (v2 -. v1)
        in distance <= tolerance

instance EqApprox Distance where
    approxEqualTolerance tol (Distance x) (Distance y) = approxEqualTolerance tol x y

instance EqApprox Area where
    approxEqualTolerance tol (Area x) (Area y) = approxEqualTolerance tol x y

instance EqApprox Angle where
    approxEqualTolerance tol (Angle x) (Angle y)
      = let Angle x' = rad x
            Angle y' = rad y
            -- If x and y are just around 0°/360° within the tolerance interval,
            -- the angles above will be apart by 2π, so we check the π rotated
            -- version here as well, avoiding the instability around 0.
            Angle x'' = rad (x+pi)
            Angle y'' = rad (y+pi)
        in approxEqualTolerance tol x' y' || approxEqualTolerance tol x'' y''

instance EqApprox Transformation where
    approxEqualTolerance tol
        (Transformation a1 b1 c1 d1 e1 f1)
        (Transformation a2 b2 c2 d2 e2 f2)
      = all (\(x,y) -> approxEqualTolerance tol x y)
            [ (a1, a2)
            , (b1, b2)
            , (c1, c2)
            , (d1, d2)
            , (e1, e2)
            , (f1, f2) ]
