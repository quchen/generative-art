module Test.Geometry.Algorithms.Dijkstra (tests) where



import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry
import Geometry.Algorithms.Dijkstra
import Numerics.VectorAnalysis

import Test.TastyAll



tests :: TestTree
tests = testGroup "Dijkstra"
    [ testMaze
    , testHill
    ]


testMaze :: TestTree
testMaze = testVisual "Path through maze" 200 200 "docs/dijkstra/maze" $ \_ -> do
    for_ [ Vec2 x y | x <- [0, 20..180], y <- [0,20..180] ] $ \p@(Vec2 x y) -> do
        Cairo.rectangle x y 20 20
        setColor (blend (maze p / 100) (mathematica97 0) (mathematica97 1))
        Cairo.fill

    let path = dijkstra Dijkstra
            { width = 200
            , height = 200
            , step = 5
            , weight = maze
            } (Vec2 30 170) (Vec2 170 170)

    smoothPathSketch 20 path
    setColor (mathematica97 3)
    Cairo.setLineWidth 5
    Cairo.stroke

    pure ()


maze :: Vec2 -> Double
maze (Vec2 x y) = cell (grid !! floor (y/20) !! floor (x/20))
  where
    cell 'X' = 100
    cell _   = 1
    grid =
        [ "XXXXXXXXXX"
        , "X    X   X"
        , "X  X X X X"
        , "X  X X X X"
        , "X  X   X X"
        , "X XXXXXX X"
        , "X X      X"
        , "X X XXXXXX"
        , "X X      X"
        , "XXXXXXXXXX"
        ]

testHill :: TestTree
testHill = testGroup "Hillclimbing"
    [ testVisual "Avoids heights"      200 200 "docs/dijkstra/avoidsHeights"  $ const avoidsHeights
    , testVisual "Avoids steep areas"  200 200 "docs/dijkstra/avoidsGradient" $ const avoidsGradient
    ]
  where
    avoidsHeights = do
        drawVectorField
        let path = dijkstra params { weight = const 1 +. 5 *. hill } (Vec2 10 10) (Vec2 190 190)
        smoothPathSketch 20 path
        setColor (mathematica97 3)
        Cairo.stroke

    avoidsGradient = do
        drawVectorField
        let path = dijkstra params { weight = \p -> 1 + norm (100 *. grad hill p) } (Vec2 10 10) (Vec2 190 190)
        smoothPathSketch 20 path
        setColor (mathematica97 3)
        Cairo.stroke

    hill (Vec2 x y) = exp (-(x-100)^2/2000)
    drawVectorField = for_ [Vec2 x y | x <- [0,5..200], y <- [0,5..200]] $ \p@(Vec2 x y) -> do
        Cairo.rectangle x y 5 5
        setColor (blend (hill p) (mathematica97 0) (mathematica97 1))
        Cairo.fill
    params = Dijkstra
        { width = 200
        , height = 200
        , step = 5
        }

smoothPathSketch :: Double -> [Vec2] -> Cairo.Render ()
smoothPathSketch d path = do
    let pathSmooth = V.toList (bezierSmoothen (V.fromList (simplifyTrajectoryRadial d (V.fromList path))))
    bezierCurveSketch pathSmooth
