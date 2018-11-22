{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Helpers where



import System.Random
import Data.List

import Geometry

import Test.Tasty.QuickCheck



instance Arbitrary Vec2 where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary
    shrink (Vec2 x y) = Vec2   0   0
                      : Vec2   1   0
                      : Vec2   0   1
                      : Vec2 (-1)  0
                      : Vec2   0 (-1)
                      : [ Vec2 x' y' | (x', y') <- shrink (x, y) ]

instance Arbitrary Angle where
    arbitrary = fmap deg (choose (0, 360-1))

newtype GaussianVec = GaussianVec Vec2

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
    arbitrary = do
        Large seed <- arbitrary
        pure (Seed seed)

gaussianVecs :: Seed -> [Vec2]
gaussianVecs (Seed seed)
  = let go (u1:u2:rest)
            | u1 <= 0 = go rest -- to avoid diverging on log(0)
            | otherwise = let root1 = sqrt (-2 * log u1)
                              pi2u2 = 2 * pi * u2
                              x = root1 * cos pi2u2
                              y = root1 * sin pi2u2
                          in Vec2 x y : go rest
        go _ = error "Can’t happen, input is infinite"
    in go (randomRs (0, 1) (mkStdGen seed))
