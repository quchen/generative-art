{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Helpers (
      GaussianVec(..)
    , LotsOfGaussianPoints(..)
) where



import Data.List
import System.Random

import Geometry
import Geometry.Coordinates.Hexagonal as Hex

import Test.Tasty.QuickCheck



instance Arbitrary Vec2 where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary
    shrink (Vec2 x y) = [ Vec2 x' y' | (x', y') <- shrink (x, y) ]

instance Arbitrary Angle where
    arbitrary = fmap deg (choose (-360, 720))
    shrink x = if getRad x == 0 then [] else [rad 0]

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

instance Arbitrary Hex where
    arbitrary = do
        q <- arbitrary
        r <- arbitrary
        pure (Hex q r (-q-r))
    shrink (Hex q r s) = [ Hex q' r' s' | (q', r', s') <- shrink (q, r, s) ]

newtype LotsOfGaussianPoints = LotsOfGaussianPoints [Vec2]
    deriving (Eq, Ord, Show)

instance Arbitrary LotsOfGaussianPoints where
    arbitrary = do
        seed <- arbitrary
        numPoints <- choose (10, 100)
        (pure . LotsOfGaussianPoints . take numPoints . gaussianVecs) seed
    shrink (LotsOfGaussianPoints xs)
      = let -- Some sufficiently chaotic seed function :-)
            seed = foldl' (\s vec -> round (normSquare vec) + s*s) 0 xs
        in map (\numPoints -> LotsOfGaussianPoints (take numPoints (gaussianVecs seed)))
               [3 .. length xs-1]

gaussianVecs :: Int -> [Vec2]
gaussianVecs seed
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
