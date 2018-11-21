module Test.Instances where



import Data.Foldable
import Data.List
import Graphics.Rendering.Cairo hiding (x,y)
import System.Random
import Data.Default.Class

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Visual.Common



instance Arbitrary Vec2 where
    arbitrary = Vec2 <$> arbitrary <*> arbitrary
    shrink (Vec2 x y) = Vec2 0 0 : [ Vec2 x' y' | (x', y') <- shrink (x, y) ]
