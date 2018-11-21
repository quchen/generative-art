{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Instances where



import Geometry

import Test.Tasty.QuickCheck



instance Arbitrary Vec2 where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary
    shrink (Vec2 x y) = Vec2 0 0 : [ Vec2 x' y' | (x', y') <- shrink (x, y) ]

instance Arbitrary Angle where
    arbitrary = fmap deg (choose (0, 360-1))
