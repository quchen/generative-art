module Test.Visual.Cut (tests) where



import Data.Foldable
import Data.List
import Graphics.Rendering.Cairo hiding (rotate, x, y)

import Draw
import Geometry

import Test.Tasty
import Test.Tasty.HUnit
import Test.Visual.Common



tests :: TestTree
tests = testGroup "Cutting things"
    [ testCase "Cut my line into pieces" lineTest
    , testCase "Convex polygon" cutSquareTest
    , testCase "Concave polygon" complicatedPolygonTest
    , testCase "Cut misses polygon" cutMissesPolygonTest
    , testCase "Cut through corner" cutThroughCornerTest
    , testCase "Edge cases" cornerCasesTest
    ]

lineTest :: IO ()
lineTest = renderAllFormats 220 100 "test/out/cut/1_line" (do
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

polyCutDraw :: Polygon -> Line -> [Polygon] -> Render ()
polyCutDraw initialPolygon scissors cutResults = do
    drawCutArrow
    drawPolygon 0 initialPolygon
    for_ (zip [0..] cutResults) (\(color, poly) -> drawPolygon color poly)
  where
    drawCutArrow = do
        setLineWidth 1
        hsva 0 0 0 1
        setDash [2,4] 0
        lineSketch scissors
        stroke
        setDash [] 0
        arrowSketch scissors def{arrowheadSize = Distance 5, arrowDrawBody = False}
        stroke
    drawPolygon color poly = do
        mmaColor color 1
        for_ (polygonEdges poly) (\edge -> do
            lineSketch edge
            arrowSketch edge def
                { arrowheadRelPos = Distance 0.45
                , arrowheadSize   = Distance 4
                }
            stroke )
        polygonSketch poly
        mmaColor color 0.1
        fill

cutSquareTest :: IO ()
cutSquareTest = renderAllFormats 170 90 "test/out/cut/2_square" (do
    let square = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        scissors = centerLine (angledLine (Vec2 25 25) (deg 20) (Distance 100))
        cutResult = cutPolygon scissors square

    polyCutDraw
        (move (Vec2 10 10) square)
        (move (Vec2 90 10) scissors)
        (move (Vec2 90 10) cutResult)

    mmaColor 1 1
    setFontSize 12
    moveTo 90 80
    showText (show (length cutResult) ++ " polygons")
    )

complicatedPolygonTest :: IO ()
complicatedPolygonTest = renderAllFormats 240 110 "test/out/cut/3_complicated" (do
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

    polyCutDraw
        (move (Vec2 50 60) polygon)
        (move (Vec2 180 60) scissors)
        (move (Vec2 180 60) cutResult)

    mmaColor 1 1
    setFontSize 12
    moveTo 140 15
    showText (show (length cutResult) ++ " polygons")
    )

cutMissesPolygonTest :: IO ()
cutMissesPolygonTest = renderAllFormats 130 90 "test/out/cut/4_miss" (do
    let scissors = Line (Vec2 0 70) (Vec2 50 60)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    polyCutDraw
        (move (Vec2 10 10) polygon)
        (move (Vec2 70 10) scissors)
        (move (Vec2 70 10) cutResult)
    )

cutThroughCornerTest :: IO ()
cutThroughCornerTest = renderAllFormats 150 90 "test/out/cut/5_through_corner" (do
    let scissors = Line (Vec2 (-15) (-15)) (Vec2 65 65)
        polygon = Polygon [Vec2 0 0, Vec2 50 0, Vec2 50 50, Vec2 0 50]
        cutResult = cutPolygon scissors polygon

    polyCutDraw
        (move (Vec2 10 20) polygon)
        (move (Vec2 80 20) scissors)
        (move (Vec2 80 20) cutResult)
    )

-- Taken from https://geidav.wordpress.com/2015/03/21/splitting-an-arbitrary-polygon-by-a-line/
cornerCasesTest :: IO ()
cornerCasesTest = renderAllFormats 240 700 "test/out/cut/6_corner_cases"
    (sequence_ (intersperse (translate 0 100) [
        ror,
        lol,
        oor,
        loo,
        roo,
        ool,
        ooo
        ]))
  where
    scissors = Line (Vec2 (-60) 0) (Vec2 60 0)
    specialCase polygon = do
        let cutResult = const [polygon] $ cutPolygon scissors polygon
        polyCutDraw
            (move (Vec2 50 50) polygon)
            (move (Vec2 170 50) scissors)
            (move (Vec2 170 50) cutResult)
    ror = specialCase (Polygon [Vec2 0 0, Vec2 40 (-40), Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) (-40)])
    lol = specialCase (Polygon [Vec2 0 0, Vec2 (-40) 40, Vec2 (-40) (-40), Vec2 40 (-40), Vec2 40 40])
    oor = specialCase (Polygon [Vec2 0 0, Vec2 0 (-40), Vec2 40 (-40), Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) 0])
    loo = specialCase (Polygon [Vec2 0 0, Vec2 (-40) 0, Vec2 (-40) (-40), Vec2 40 (-40), Vec2 40 40, Vec2 0 40])
    roo = specialCase (Polygon [Vec2 0 0, Vec2 40 0, Vec2 40 40, Vec2 (-40) 40, Vec2 (-40) (-40), Vec2 0 (-40)])
    ool = specialCase (Polygon [Vec2 0 0, Vec2 0 40, Vec2 (-40) 40, Vec2 (-40) (-40), Vec2 40 (-40), Vec2 40 0])
    ooo = specialCase (Polygon [Vec2 (-40) 0, Vec2 0 0, Vec2 40 0, Vec2 0 40])
