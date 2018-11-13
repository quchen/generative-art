module Test.Visual.Reflection (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Test.Visual.Common



tests :: TestTree
tests = testCase "Reflection of rays on a mirror" testReflection

testReflection :: IO ()
testReflection = renderAllFormats 520 300 "test/out/reflection" (do

    let mirror = angledLine (Vec2 10 100) (deg 10) (Distance 510)

    setLineWidth 2
    hsva 0 0 0 0.5
    lineSketch mirror
    stroke

    do
        let rayOrigin = Vec2 180 250
        setLineWidth 1
        hsva 0 1 0.7 1
        circleSketch rayOrigin (Distance 5)
        stroke
        for_ (zip [-135,-120.. -10] [0,6..]) (\(angleDeg, colorDeg) -> do
            let rayRaw = angledLine rayOrigin (deg angleDeg) (Distance 100)
                (Line _ reflectedRayEnd, iPoint, _) = reflection rayRaw mirror
                ray = Line rayOrigin iPoint
                ray' = Line iPoint reflectedRayEnd
            hsva colorDeg 1 0.7 0.7
            lineSketch ray
            lineSketch ray'
            stroke )
    do
        let rayOrigin = Vec2 350 30
        setLineWidth 1
        hsva 180 1 0.7 1
        circleSketch rayOrigin (Distance 5)
        stroke
        for_ (zip [-135,-120.. -10] [180,180+6..]) (\(angleDeg, colorDeg) -> do
            let rayRaw = angledLine rayOrigin (deg angleDeg) (Distance 100)
                (Line _ reflectedRayEnd, iPoint, _) = reflection rayRaw mirror
                ray = Line rayOrigin iPoint
                ray' = Line iPoint reflectedRayEnd
            hsva colorDeg 1 0.7 0.7
            lineSketch ray
            lineSketch ray'
            stroke )

    )
