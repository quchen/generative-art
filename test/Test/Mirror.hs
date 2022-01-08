module Test.Mirror (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testCase "Mirror along an axis" mirrorPointsTest

originalC, mirroredC :: Double -> Render ()
originalC = mmaColor 0
mirroredC = mmaColor 1

setMirrorStyle :: Render ()
setMirrorStyle = hsva 0 0 0 0.5 >> setDash [5,5] 0 >> setLineWidth 1

mirrorPointsTest :: IO ()
mirrorPointsTest = renderAllFormats 550 550 "docs/geometry/mirror" (do
    Cairo.translate 10 20
    mirror1
    Cairo.translate 0 230
    mirror2
    )

mirror1 :: Render ()
mirror1 = do
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
        ]
  where
    testDraw :: Line -> [Vec2] -> Render ()
    testDraw mirror ps = do
        setLineWidth 1

        restoreStateAfter $ do
            setMirrorStyle
            lineSketch mirror
            stroke

        for_ ps (\p -> do
            let p' = mirrorAlong mirror p

            mirroredC 1
            crossSketch p (Distance 5)
            stroke
            originalC 1
            circleSketch p' (Distance 5)
            stroke

            hsva 0 0 0 0.5
            arrowSketch (Line p p') def
            stroke )

mirror2 :: Render ()
mirror2 = do
    let mirror = angledLine (Vec2 10 100) (deg 10) (Distance 510)

    restoreStateAfter $ do
        setMirrorStyle
        lineSketch mirror
        stroke

    setFontSize 12
    originalC 1 >> moveTo 180 30 >> showText "Original"
    mirroredC 1 >> moveTo 180 45 >> showText "Mirrored"

    setLineWidth 1
    let mirrorLineTest line = do
            let mirrored = mirrorAlong mirror line
            originalC 1 >> arrowSketch line def >> stroke
            mirroredC 1 >> arrowSketch mirrored def >> stroke
    mirrorLineTest (angledLine (Vec2 50 10) (deg 20) (Distance 100))
    mirrorLineTest (angledLine (Vec2 150 10) (deg 90) (Distance 100))
    mirrorLineTest (angledLine (Vec2 160 10) (deg 90) (Distance 150))
    mirrorLineTest (angledLine (Vec2 300 10) (deg 120) (Distance 180))
    mirrorLineTest (angledLine (Vec2 250 160) (deg 0) (Distance 200))
    mirrorLineTest (angledLine (Vec2 120 110) (deg 180) (Distance 100))

    let mirrorPolygonTest poly = do
            let mirrored = mirrorAlong mirror poly
            originalC 1 >> polygonSketch poly >> strokePreserve >> originalC 0.1 >> fill
            mirroredC 1 >> polygonSketch mirrored >> strokePreserve >> mirroredC 0.1 >> fill
    mirrorPolygonTest (Polygon [Vec2 350 200, Vec2 400 220, Vec2 380 240, Vec2 420 220, Vec2 420 200])
    mirrorPolygonTest (Polygon [Vec2 339 56, Vec2 310 110, Vec2 370 82, Vec2 300 70, Vec2 348 118])
