{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Properties (tests) where



import Geometry

import Test.Tasty
import Test.QuickCheck
import Test.Tasty.QuickCheck



tests :: TestTree
tests = testGroup "Properties"
    [ angleBetweenTest ]

newtype Tolerance = Tolerance Double

approxEqualTolerance :: Tolerance -> Double -> Double -> Bool
approxEqualTolerance (Tolerance tolerance) reference value
  = abs (reference - value) / reference <= tolerance

(~==) :: Double -> Double -> Bool
(~==) = approxEqualTolerance (Tolerance 1e-10)

angleBetweenTest :: TestTree
angleBetweenTest = testProperty "Angle between two lines"
    (\angle1@(Angle angle1Raw) angle2@(Angle angle2Raw) ->
        let line1 = angledLine (Vec2 0 0) angle1 (Distance 1)
            line2 = angledLine (Vec2 0 0) angle2 (Distance 1)
            actualAngle@(Angle actualAngleRaw) = angleBetween line1 line2
            expectedAngleRaw = angle2Raw - angle1Raw
            expectedAngle = rad expectedAngleRaw
        in counterexample
            (unlines ["Counterexample:"
                     , "  Line 1: " ++ show line1 ++ " (angle: " ++ show angle1 ++ ")"
                     , "  Line 2: " ++ show line2 ++ " (angle: " ++ show angle2 ++ ")"
                     , "  Δ actual:   " ++ show actualAngle
                     , "  Δ expected: " ++ show expectedAngle
                     ])
            (expectedAngleRaw ~== actualAngleRaw) )

instance Arbitrary Angle where
    arbitrary = fmap deg (choose (0, 360-1))
