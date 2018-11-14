module Test.Visual.Cut (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Test.Visual.Common



tests :: TestTree
tests = testCase "Cut my line into pieces" cutTest

cutTest :: IO ()
cutTest = renderAllFormats 230 380 "test/out/cut" (do
    translate 10 40
    cutLineDrawing
    translate 120 110
    cutSquareDrawing
    translate 0 110
    cutComplicatedPolygon
    )

cutLineDrawing :: Render ()
cutLineDrawing = do
    let paper = angledLine (Vec2 0 0) (deg 20) (Distance 100)
        scissors = perpendicularBisector paper
        Cut paperStart p paperEnd = cutLine scissors paper

    setLineWidth 1
    hsva 0 0 0 1
    setDash [2,4] 0
    lineSketch scissors
    stroke
    setDash [] 0

    setLineWidth 3
    mmaColor 0 1
    lineSketch (Line paperStart p)
    stroke
    mmaColor 3 1
    lineSketch (Line p paperEnd)
    stroke

    mmaColor 1 1
    setFontSize 12
    moveTo 60 10
    showText "Cut my line in two pieces"

polyCutDraw :: Line -> [Polygon] -> Render ()
polyCutDraw scissors cutResults = do

    setLineWidth 1
    hsva 0 0 0 1
    setDash [2,4] 0
    lineSketch scissors
    stroke
    setDash [] 0


    for_ (zip [0..] cutResults) (\(color, poly) -> do
        mmaColor color 1
        for_ (polygonEdges poly) (\edge -> do
            lineSketch edge
            let Line start end = edge
            arrowHead (Line start (mulVec2 0.5 (end `addVec2` start))) (Distance 7)
            stroke
            )

        polygonSketch poly
        mmaColor color 0.1
        fill
        )

cutSquareDrawing :: Render ()
cutSquareDrawing = do
    let square = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        scissors = centerLine (angledLine (Vec2 25 25) (deg 20) (Distance 100))
        cutResult = cutPolygon scissors square
    polyCutDraw scissors (move (Vec2 (-80) 0) square : cutResult)

    mmaColor 1 1
    setFontSize 12
    moveTo (-10) 70
    showText (show (length cutResult) ++ " polygons")
    -- for_ cutResult (\poly -> translate 0 60 >> polygonSketch poly >> hsva 0 0 0 1 >> stroke)

cutComplicatedPolygon :: Render ()
cutComplicatedPolygon = do
    let polygon = Polygon (scanl addVec2 (Vec2 0 0)
            [ Vec2 60 0
            , Vec2 0 40
            , Vec2 (-20) 0
            , Vec2 0 (-20)
            , Vec2 (-20) 0
            , Vec2 0 40
            , Vec2 40 0
            , Vec2 0 20
            , Vec2 (-60) 0 ])
        scissors = centerLine (angledLine (Vec2 (60/2) (60/2)) (deg 130) (Distance 150))
        cutResult = cutPolygon scissors polygon

    polyCutDraw scissors (move (Vec2 (-100) 0) polygon : cutResult)

    mmaColor 1 1
    setFontSize 12
    moveTo (-10) 100
    showText (show (length cutResult) ++ " polygons")
