{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Geometry.Algorithms.WaveFunctionCollapse (tests) where



import Control.Monad.ST (runST)
import qualified Data.MultiSet as M
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)
import System.Random.MWC (create, initialize)
import Text.Printf (printf)

import Draw
import Draw.Grid
import Geometry
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

exampleGrid :: RectilinearGrid XO
exampleGrid = from2DList
    [ [ X, O, X ]
    , [ X, X, O ]
    , [ X, O, O ]
    ]

testDrawGrid :: TestTree
testDrawGrid = testVisual "3x3 Grid" 240 240 "docs/wave_function_collapse/grid" $ \(w, h) ->
    drawGrid (w, h) exampleGrid

testDuplicate :: TestTree
testDuplicate = testVisual "Duplicated grid" 240 240 "docs/wave_function_collapse/grid_duplicate" $ \(w, h) ->
    drawGrid (w, h) (duplicate exampleGrid)

testStencil :: TestTree
testStencil = testVisual "Create stencils from grid" 240 240 "docs/wave_function_collapse/grid_stencil" $ \(w, h) ->
    drawGrid (w, h) (extend stencil3x3 exampleGrid)

testStencilToGrid :: TestTree
testStencilToGrid = testVisual "Create stencils from grid" 240 240 "docs/wave_function_collapse/grid_stencil_grid" $ \(w, h) ->
    drawGrid (w, h) (stencilToGrid <$> extend stencil3x3 exampleGrid)

testCollapse :: TestTree
testCollapse = testVisual "Collapse grid at position" 240 240 "docs/wave_function_collapse/grid_collapse" $ \(w, h) ->
    drawGrid (w, h) $ runST $ do
        gen <- create
        let settings = settingsFromGrid (6, 6) example
            initial = initialGrid settings
        Just grid <- pickMin gen initial
        propagate (wfcLocalProjection settings) <$> collapse gen grid

testPropagate :: TestTree
testPropagate = testGroup "Propagation"
    [ testVisual (printf "Generation %i" i) 720 720 (printf "docs/wave_function_collapse/propagation_%i" i) $ \(w, h) ->
        drawGrid (w, h) (getTouched <$> grid)
    | (i, grid) <- take 7 $ zip [0 :: Int ..] generations
    ]
  where
    settings = settingsFromGrid (6, 6) example
    generations = iterate (extend (wfcLocalProjection settings)) $ runST $ do
        gen <- create
        let initial = initialGrid settings
        mapCurrent (Touched . getTouched) . fmap Untouched <$> collapse gen initial

testWaveFunctionCollapse :: TestTree
testWaveFunctionCollapse = testCase "WaveFunctionCollapse" $ do
    let w, h :: Num a => a
        w = 480
        h = 480
    for_ (zip [1 :: Int ..] steps) $ \(i, step) ->
        renderAllFormats w h (printf "docs/wave_function_collapse/wave_function_collapse_%02i" i) $
            drawGrid (w, h) (averageColor . fmap extractStencil . M.toList <$> step)
  where
    steps = runST $ do
        gen <- initialize (V.fromList [5])
        wfc (settingsFromGrid (20, 20) example) gen
    averageColor = average . fmap toColor

instance DrawToSize (Color Double) where
    drawToSize (w, h) color = cairoScope $ do
        sketch (Polygon [Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h])
        setColor color
        fill

instance DrawToSize XO where
    drawToSize (w, h) = drawToSize (w, h) . toColor

data XO = X | O deriving (Eq, Ord, Show)

toColor :: XO -> Color Double
toColor = \case
    X -> blend 0.1 white $ mathematica97 1
    O -> blend 0.1 white $ mathematica97 0

example :: RectilinearGrid XO
example = from2DList
    [ [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ X, X, X, O, X, X, X, X, X, O, X, X, X ]
    , [ O, O, O, O, O, O, O, O, O, O, O, O, O ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    , [ X, X, X, X, X, X, O, X, X, X, X, X, X ]
    ]

from2DList :: [[a]] -> RectilinearGrid a
from2DList = fromList . concat . zipWith (\y vs -> fmap (\(x, v) -> ((x, y), v)) vs) [0..] . fmap (zip [0..])
