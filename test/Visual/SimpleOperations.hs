module Visual.SimpleOperations (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Visual.Common



tests :: TestTree
tests = testCase "Simple operations" testSimple

testSimple = renderAllFormats 320 240 "test/out/simple_operations" (do
    translate 10 20 >> perpendicularBisectorTest >> identityMatrix )

perpendicularBisectorTest :: Render ()
perpendicularBisectorTest = do
    let line = angledLine (Vec2 0 0) (deg 30) (Distance 50)
        bisector = perpendicularBisector line

    setLineWidth 1
    mmaColor 0 1
    lineSketch line
    stroke
    mmaColor 1 1
    lineSketch bisector
    stroke

    setFontSize 10
    moveTo 40 10
    showText "Perpendicular bisector"
