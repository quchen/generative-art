{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Arbitrary (
    Gaussian(..)
) where



import Data.Coerce

import Geometry
import Geometry.Coordinates.Hexagonal as Hex

import Test.Tasty.QuickCheck



instance Arbitrary Vec2 where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary
    shrink (Vec2 x y) = [ Vec2 x' y' | (x', y') <- shrink (x, y) ]

instance Arbitrary Angle where
    arbitrary = fmap deg (choose (-360, 720))
    shrink x = if getRad x == 0 then [] else [rad 0]

instance Arbitrary Hex where
    arbitrary = do
        q <- arbitrary
        r <- arbitrary
        pure (Hex q r (-q-r))
    shrink (Hex q r s) = [ Hex q' r' s' | (q', r', s') <- shrink (q, r, s) ]

-- | Modifier for 'Vec2' whose 'Arbitrary' instance is Gaussian (normal).
newtype Gaussian = Gaussian Vec2
    deriving (Eq, Ord, Show)

instance Arbitrary Gaussian where
    arbitrary = do
        u1 <- choose (0, 1)
        if u1 <= 0 -- to avoid diverging on log(0)
            then arbitrary
            else do
                angle <- choose (0, 2*pi)
                let radius = sqrt (-2 * log u1)
                    x = radius * cos angle
                    y = radius * sin angle
                pure (Gaussian (Vec2 x y))
    shrink (Gaussian vec) = coerce (shrink vec)
