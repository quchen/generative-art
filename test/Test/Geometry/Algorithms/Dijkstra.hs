module Test.Geometry.Algorithms.Dijkstra (tests) where

import Control.Monad.ST
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC
import System.Random.MWC.Distributions

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

    let path = V.fromList $ dijkstra Dijkstra
            { width = 200
            , height = 200
            , step = 5
            , weight = maze
            } (Vec2 30 170) (Vec2 170 170)

    let path' = runST $ do
            gen <- create
            iterateM 30000 (optimizeTrajectory maze 0.01 gen) path

    drawPath path'


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

    avoidsHeights = testCase (const 1 +. 5 *. hill)
    avoidsGradient = testCase (\p -> 1 + norm (100 *. grad hill p))

    testCase costFunction = do
        drawVectorField
        let weight = costFunction
            path = dijkstra Dijkstra{..} (Vec2 10 10) (Vec2 190 190)
            path' = runST $ do
                gen <- create
                iterateM 20000 (optimizeTrajectory weight 0.005 gen) (V.fromList path)

        drawPath path'

    hill (Vec2 x _) = exp (-(x-100)^2/2000)
    drawVectorField = for_ [Vec2 x y | x <- [0,5..200], y <- [0,5..200]] $ \p@(Vec2 x y) -> do
        Cairo.rectangle x y 5 5
        setColor (blend (hill p) (mathematica97 0) (mathematica97 1))
        Cairo.fill
    params = Dijkstra
        { width = 200
        , height = 200
        , step = 10
        }

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

optimizeTrajectory :: (Vec2 -> Double) -> Double -> GenST s -> V.Vector Vec2 -> ST s (V.Vector Vec2)
optimizeTrajectory f d gen ps = do
    pos <- uniformRM (1, V.length ps - 2) gen
    nudge <- normal 0 d gen
    let item = ps V.! pos
        [a, b, c] = V.toList (V.slice (pos-1) 3 ps)
        nudgeDirection = let Vec2 x y = (c -. a) /. 2 in Vec2 y (-x)
        b' = (a+.c) /. 2 +. nudge *. nudgeDirection
    pure $ if trajectoryLength a b' c < trajectoryLength a b c
        then ps V.// [(pos, b')]
        else ps
  where
    trajectoryLength a b c = (f a + f b) * norm (b -. a) + (f b + f c) * norm (c -. b)

gaussianVec2
    :: Vec2 -- ^ Mean
    -> Double -- ^ Standard deviation
    -> GenST s
    -> ST s Vec2
gaussianVec2 (Vec2 muX muY) sigma gen = Vec2 <$> normal muX sigma gen <*> normal muY sigma gen

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = pure a
iterateM n f a = f a >>= iterateM (n-1) f
