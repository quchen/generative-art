module Test.Geometry.Algorithms.Dijkstra (tests) where

import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry
import Geometry.Algorithms.Path.Dijkstra
import Geometry.Algorithms.Path.Optimize
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

    let path = optimizePath 30000 0.01 maze $ dijkstra Dijkstra
            { width = 200
            , height = 200
            , step = 5
            , costFunction = maze
            } (Vec2 30 170) (Vec2 170 170)

    drawPath path


maze :: Vec2 -> Double
maze (Vec2 x y)
    | x >= 0 && y >= 0 && x < 200 && y < 200 = cell (grid !! floor (y/20) !! floor (x/20))
    | otherwise = 100
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
    [ testVisual "Avoids heights"      width height "docs/dijkstra/avoidsHeights"  $ const avoidsHeights
    , testVisual "Avoids steep areas"  width height "docs/dijkstra/avoidsGradient" $ const avoidsGradient
    ]
  where
    width, height :: Num a => a
    width = 200
    height = 200
    step = 5

    avoidsHeights = testWith (const 1 +. 5 *. hill)
    avoidsGradient = testWith (\p -> 1 + norm (100 *. grad hill p))

    testWith costFunction = do
        drawVectorField
        let path = optimizePath 20000 0.005 costFunction $ dijkstra Dijkstra{..} (Vec2 10 10) (Vec2 190 190)

        drawPath path

    hill (Vec2 x _) = exp (-(x-100)^2/2000)
    drawVectorField = for_ [Vec2 x y | x <- [0,5..200], y <- [0,5..200]] $ \p@(Vec2 x y) -> do
        Cairo.rectangle x y 5 5
        setColor (blend (hill p) (mathematica97 0) (mathematica97 1))
        Cairo.fill

drawPath :: Foldable f => f Vec2 -> Cairo.Render ()
drawPath path = do
    setColor (mathematica97 3)
    pathSketch path
    Cairo.setLineWidth 1
    Cairo.stroke
    for_ (toList path) $ \p -> do
        circleSketch p 1.5
        setColor (mathematica97 3)
        Cairo.stroke

