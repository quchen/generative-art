module Visual.Mirror (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry

import Visual.Common
import Test.Tasty
import Test.Tasty.HUnit




tests :: TestTree
tests = testCase "Mirror along an axis" testMirror

testMirror :: IO ()
testMirror = renderAllFormats 250 200 "test/out/mirror" (do
    testDraw
        (angledLine (Vec2 10 100) (Angle 0) (Distance 100))
        [ Vec2 40 10
        , Vec2 20 20
        , Vec2 60 60
        , Vec2 80 160
        ]
    testDraw
        (angledLine (Vec2 150 100) (deg (-30)) (Distance 100))
        [ Vec2 160 10
        , Vec2 140 20
        , Vec2 180 60
        , Vec2 230 100
        ])

testDraw :: Line -> [Vec2] -> Render ()
testDraw mirror ps = do
    setLineWidth 2
    hsva 0 0 0 1
    lineSketch mirror
    stroke

    for_ ps (\p -> do
        let p' = mirrorAlong mirror p

        setLineWidth 1
        hsva 0 1 0.7 1
        crossSketch p (Distance 5)
        stroke
        hsva 120 1 0.7 1
        circleSketch p' (Distance 5)
        stroke

        hsva 120 0 0 0.5
        arrowSketch (Line p p')
        stroke )
