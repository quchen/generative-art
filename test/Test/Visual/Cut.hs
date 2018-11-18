module Test.Visual.Cut (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo hiding (rotate, x, y)

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Test.Visual.Common



tests :: TestTree
tests = testGroup "Cutting things"
    [ testCase "Cut my line into pieces" lineTest
    , testCase "Convex polygon" cutSquareDrawingTest
    , testCase "Concave polygon" complicatedPolygonTest
    , testCase "Cut misses polygon" cutMissesPolygonTest
    , testCase "Cut through corner" cutThroughCorner
    ]

lineTest :: IO ()
lineTest = renderAllFormats 220 100 "test/out/cut_1_line" (do
    translate 3 32
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
    )

polyCutDraw :: Line -> [Polygon] -> Render ()
polyCutDraw scissors cutResults = do

    do -- Cut arrow
        setLineWidth 1
        hsva 0 0 0 1
        setDash [2,4] 0
        lineSketch scissors
        stroke
        setDash [] 0
        arrowHead scissors (Distance 5)
        stroke

    for_ (zip [0..] cutResults) (\(color, poly) -> do
        mmaColor color 1
        for_ (polygonEdges poly) (\edge -> do
            lineSketch edge
            let Line start end = edge
            arrowHead (Line start (mulVec2 0.5 (end `addVec2` start))) (Distance 4)
            stroke
            )

        polygonSketch poly
        mmaColor color 0.1
        fill
        )

cutSquareDrawingTest :: IO ()
cutSquareDrawingTest = renderAllFormats 170 90 "test/out/cut_2_square" (do
    translate 90 10
    let square = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        scissors = centerLine (angledLine (Vec2 25 25) (deg 20) (Distance 100))
        cutResult = cutPolygon scissors square
    polyCutDraw scissors (move (Vec2 (-80) 0) square : cutResult)

    mmaColor 1 1
    setFontSize 12
    moveTo (-10) 70
    showText (show (length cutResult) ++ " polygons")
    )

complicatedPolygonTest :: IO ()
complicatedPolygonTest = renderAllFormats 230 130 "test/out/cut_3_complicated" (do
    translate 170 60
    let polygon = spiral 9
        spiral n = Polygon (scanl addVec2 (Vec2 0 0) relativeSpiral)
          where
            instructions = concat [ zip [1..n] (repeat turnLeft)
                                  , [(1, turnLeft)]
                                  , [(n-1, turnRight)]
                                  , zip [n-3, n-4 .. 1] (repeat turnRight)
                                  ]
            relativeSpiral = go instructions (Vec2 1 0)
              where
                go [] _dir = []
                go ((len, rotate) : rest) direction = mulVec2 (10*len) direction : go rest (rotate direction)
            turnLeft  (Vec2 x y) = Vec2   y  (-x)
            turnRight (Vec2 x y) = Vec2 (-y)   x
        scissors = centerLine (angledLine (Vec2 (-5) (-5)) (deg 140) (Distance 150))
        cutResult = cutPolygon scissors polygon

    polyCutDraw scissors (move (Vec2 (-120) 0) polygon : cutResult)

    mmaColor 1 1
    setFontSize 12
    moveTo (-40) 60
    showText (show (length cutResult) ++ " polygons")
    )

cutMissesPolygonTest :: IO ()
cutMissesPolygonTest = renderAllFormats 130 90 "test/out/cut_4_miss" (do
    translate 70 10
    let scissors = Line (Vec2 0 70) (Vec2 50 60)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    polyCutDraw scissors (move (Vec2 (-60) 0) polygon : cutResult)
    )

cutThroughCorner :: IO ()
cutThroughCorner = renderAllFormats 140 100 "test/out/cut_5_through_corner" (do
    translate 70 30
    let scissors = Line (Vec2 (-15) (-15)) (Vec2 65 65)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    polyCutDraw scissors (move (Vec2 (-60) 0) polygon : cutResult)
    )

-- TODO: corner cases from https://geidav.wordpress.com/2015/03/21/splitting-an-arbitrary-polygon-by-a-line/
