module Visual.Reflection (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry

import Visual.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Reflection of rays on a mirror"
    [ testCase "#1" testReflection
    , testCase "#2" testReflection2
    ]

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
                (Line _ reflectedRayEnd, iPoint, _, _) = reflection rayRaw mirror
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
                (Line _ reflectedRayEnd, iPoint, _, _) = reflection rayRaw mirror
                ray = Line rayOrigin iPoint
                ray' = Line iPoint reflectedRayEnd
            hsva colorDeg 1 0.7 0.7
            lineSketch ray
            lineSketch ray'
            stroke )

    )



testReflection2 :: IO ()
testReflection2 = renderAllFormats 520 300 "test/out/mirror2" (do
    let mirror = angledLine (Vec2 10 100) (deg 10) (Distance 510)

    setLineWidth 2
    hsva 0 0 0 0.5
    lineSketch mirror
    stroke

    setLineWidth 1
    let mirrorLineTest line = do
            let mirrored = mirrorAlong mirror line
            hsva 120 1 0.7 1 >> arrowSketch line >> stroke
            hsva 0 1 0.7 1 >> arrowSketch mirrored >> stroke
    mirrorLineTest (angledLine (Vec2 50 10) (deg (20)) (Distance 100))
    mirrorLineTest (angledLine (Vec2 150 10) (deg 90) (Distance 100))
    mirrorLineTest (angledLine (Vec2 160 10) (deg 90) (Distance 150))
    mirrorLineTest (angledLine (Vec2 300 10) (deg 120) (Distance 180))
    mirrorLineTest (angledLine (Vec2 250 160) (deg 0) (Distance 200))
    mirrorLineTest (angledLine (Vec2 120 110) (deg 180) (Distance 100))

    let mirrorPolygonTest poly = do
            let mirrored = mirrorAlong mirror poly
            hsva 120 1 0.7 1 >> polygonSketch poly >> strokePreserve >> hsva 120 1 0.7 0.1 >> fill
            hsva 0 1 0.7 1 >> polygonSketch mirrored >> strokePreserve >> hsva 0 1 0.7 0.1 >> fill
    mirrorPolygonTest (Polygon [Vec2 350 200, Vec2 400 220, Vec2 380 240, Vec2 420 220, Vec2 420 200])
    mirrorPolygonTest (Polygon [Vec2 339 56, Vec2 310 110, Vec2 370 82, Vec2 300 70, Vec2 348 118])
    )
