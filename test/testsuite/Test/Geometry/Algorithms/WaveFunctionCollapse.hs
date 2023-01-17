{-# LANGUAGE FlexibleInstances #-}
module Test.Geometry.Algorithms.WaveFunctionCollapse (tests) where



import Control.Monad.ST (runST)
import qualified Data.MultiSet as M
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)
import System.Random.MWC (create, initialize)

import Draw
import Draw.Grid
import Geometry hiding (Grid)
import Geometry.Algorithms.WaveFunctionCollapse as Wfc

import Test.TastyAll



tests :: TestTree
tests = testGroup "WaveFunctionCollapse algorithm"
    [ testDrawGrid
    , testDuplicate
    , testStencil
    , testStencilToGrid
    , testPropagate
    , testCollapse
    , localOption (Timeout (20*10^6) "20s") testWaveFunctionCollapse
    ]

exampleGrid :: Grid XO
exampleGrid = fromList
    [ [ X, O, X ]
    , [ X, X, O ]
    , [ X, O, O ]
    ]

testDrawGrid :: TestTree
testDrawGrid = testVisual "3x3 Grid" 240 240 "docs/grid" $ \(w, h) ->
    drawGrid (w, h) exampleGrid

testDuplicate :: TestTree
testDuplicate = testVisual "Duplicated grid" 240 240 "docs/grid_duplicate" $ \(w, h) ->
    drawGrid (w, h) (duplicate exampleGrid)

testStencil :: TestTree
testStencil = testVisual "Create stencils from grid" 240 240 "docs/grid_stencil" $ \(w, h) ->
    drawGrid (w, h) (extend stencil3x3 exampleGrid)

testStencilToGrid :: TestTree
testStencilToGrid = testVisual "Create stencils from grid" 240 240 "docs/grid_stencil_grid" $ \(w, h) ->
    drawGrid (w, h) (stencilToGrid <$> extend stencil3x3 exampleGrid)

testCollapse :: TestTree
testCollapse = testVisual "Collapse grid at position" 240 240 "docs/grid_collapse" $ \(w, h) ->
    drawGrid (w, h) $ runST $ do
        gen <- create
        let settings = settingsFromGrid example
            initial = initialGrid settings 6 6
        Just grid <- pickMin gen initial
        propagate (wfcLocalProjection settings) <$> collapse gen grid

testPropagate :: TestTree
testPropagate = testGroup "Propagation"
    [ testVisual ("Generation " ++ show i) 720 720 ("docs/propagation_" ++ show i) $ \(w, h) ->
        drawGrid (w, h) grid
    | (i, grid) <- take 7 $ zip [0..] generations
    ]
  where
    settings = settingsFromGrid example
    generations = iterate (extend (wfcLocalProjection settings)) $ runST $ do
        gen <- create
        let initial = initialGrid settings 6 6
        collapse gen initial

testWaveFunctionCollapse :: TestTree
testWaveFunctionCollapse = testGroup "WaveFunctionCollapse"
    [ testVisual ("WaveFunctionCollapse step " ++ show i) 480 480 ("docs/wave_function_collapse_" ++ show i) $ \(w, h) ->
        drawGrid (w, h) (averageColor . fmap extractStencil . M.toList <$> step)
    | (i, step) <- zip [1..] steps
    ]
  where
    steps = runST $ do
        gen <- initialize (V.fromList [5])
        wfc (settingsFromGrid example) 20 20 gen
    averageColor = average . fmap toColor

instance DrawToSize (Color Double) where
    drawToSize (w, h) color = cairoScope $ do
        sketch (Polygon [Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h])
        setColor black
        strokePreserve
        setColor color
        fill

instance DrawToSize XO where
    drawToSize (w, h) = drawToSize (w, h) . toColor

data XO = X | O deriving (Eq, Ord, Show)

toColor :: XO -> Color Double
toColor = \case
    X -> rgb 1 1 1
    O -> rgb 1 0 0

example :: Grid XO
example = fromList
    [ [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ O, O, O, O, O, O, O, O, O, O, O, O, O ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    ]
