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
    shrink x =
        let r = getRad x
        in if
            -- Angle 0 is as simple as it gets
            | r == 0 -> []

            -- For single turns, try with zero angle
            | 0 <= r && r < 2*pi -> [rad 0]

            -- For non-normalized angles, normalize them
            | otherwise -> [normalizeAngle (rad 0) x]

-- | Non-singular transformation
instance Arbitrary Transformation where
    arbitrary = suchThatMap arbitrary (\(a,b,c,d,e,f) -> if det (Vec2 a b) (Vec2 d e) == 0 then Nothing else Just (Transformation a b c d e f))
    shrink trafo
        | trafo /= mempty = [mempty]
        | otherwise = []

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
