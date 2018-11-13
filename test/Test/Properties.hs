{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Properties (tests) where



import Control.Applicative

import Geometry

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck



tests :: TestTree
tests = testGroup "Properties"
    [ angleBetweenTest
    , areaTest ]

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

areaTest :: TestTree
areaTest = testGroup "Area"
    [ parallelogram
    , square ]
  where
    coord = choose (-100, 100 :: Double)
    vec2 = liftA2 Vec2 coord coord

    square = testProperty "Square" (forAll
        ((,,,,) <$> vec2 <*> vec2 <*> vec2 <*> vec2 <*> arbitrary)
        (\((Vec2 x1 y1), (Vec2 x2 y2), center, moveVec, angle) ->
            let poly = (move moveVec . rotateAround center angle . Polygon)
                           [Vec2 x1 y1, Vec2 x1 y2, Vec2 x2 y2, Vec2 x2 y1]
                Area actual = polygonArea poly
                expected = abs ((x2-x1) * (y2-y1))
            in actual ~== expected ))

    parallelogram = testProperty "Parallelogram" (forAll
        ((,) <$> vec2 <*> vec2)
        (\(v1, v2) ->
            let Area actual = polygonArea (Polygon [zero, v1, v1 `addVec2` v2, v2])
                zero = Vec2 0 0
                Angle rawAngleBetween = angleBetween (Line zero v1) (Line zero v2)
                Distance v1norm = norm v1
                Distance v2norm = norm v2
                expected = abs (v1norm * v2norm * sin rawAngleBetween)
            in actual ~== expected ))
