module Test.Geometry.Algorithms.WaveFunctionCollapse (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)

import Draw
import Geometry hiding (Grid)
import Geometry.Algorithms.WaveFunctionCollapse as Wfc

import Test.TastyAll
import System.Random.MWC (create)
import Control.Monad.ST (runST)

tests :: TestTree
tests = testGroup "WaveFunctionCollapse algorithm"
    [ testDrawGrid
    , testDuplicate
    , testPropagate
    ]

exampleGrid :: Grid XO
exampleGrid = fromListG
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

testPropagate :: TestTree
testPropagate = testGroup "Propagation"
    [ testVisual "Iniital state" 720 720 "docs/propagation_0" $ \(w, h) ->
        drawGrid (w, h) grid
    , testVisual "First iteration" 720 720 "docs/propagation_1" $ \(w, h) ->
        drawGrid (w, h) $ extend (wfcLocalProjection settings) grid
    , testVisual "Second iteration" 720 720 "docs/propagation_2" $ \(w, h) ->
        drawGrid (w, h) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) grid
    , testVisual "Third iteration" 720 720 "docs/propagation_3" $ \(w, h) ->
        drawGrid (w, h) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) grid
    , testVisual "Fourth iteration" 720 720 "docs/propagation_4" $ \(w, h) ->
        drawGrid (w, h) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) grid
    , testVisual "Fifth iteration" 720 720 "docs/propagation_5" $ \(w, h) ->
        drawGrid (w, h) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) $ extend (wfcLocalProjection settings) grid
    ]
  where
    settings = settingsFromGrid example
    grid = runST $ do
        gen <- create
        let initial = initialGrid settings 5 5
        collapse gen initial

zipperLength :: Zipper a -> Int
zipperLength = length . Wfc.toList

extents :: Grid a -> (Int, Int)
extents (Grid z) = (zipperLength (extractZ z), zipperLength z)

drawGrid :: DrawToSize a => (Double, Double) -> Grid a -> Cairo.Render ()
drawGrid (w, h) grid@(Grid zz) = cairoScope $ do
    Cairo.translate margin margin
    for_ (zip (Wfc.toList zz) [0..]) $ \(row, y) ->
        for_ (zip (Wfc.toList row) [0..]) $ \(cell, x) -> do
            drawCell x y cell
    highlightCurrent
  where
    (gridW, gridH) = extents grid
    margin = (w + h) * 0.05
    cellW = (w-2*margin) / fromIntegral gridW
    cellH = (h-2*margin) / fromIntegral gridH
    drawCell x y content = cairoScope $ do
        Cairo.translate (x*cellW) (y*cellH)
        drawToSize (cellW, cellH) content
    highlightCurrent = do
        let currentY = case zz of
                Zipper as _ _ -> length as
            currentX = case extractZ zz of
                Zipper as _ _ -> length as
        Cairo.translate (fromIntegral currentX * cellW) (fromIntegral currentY * cellH)
        Cairo.setLineWidth 2
        setColor $ rgb 0 0 1
        sketch (Polygon [Vec2 0 0, Vec2 cellW 0, Vec2 cellW cellH, Vec2 0 cellH])
        Cairo.stroke


class DrawToSize a where
    drawToSize :: (Double, Double) -> a -> Render ()

instance DrawToSize XO where
    drawToSize (w, h) xo = cairoScope $ do
        sketch (Polygon [Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h])
        setColor black
        strokePreserve
        setColor $ case xo of
            X -> white
            O -> rgb 1 0 0
        fill

instance DrawToSize a => DrawToSize (Grid a) where
    drawToSize = drawGrid

instance DrawToSize a => DrawToSize (Stencil3x3 a) where
    drawToSize (w, h) = drawGrid (w, h) . stencilToGrid

instance DrawToSize a => DrawToSize [a] where
    drawToSize (w, h) items = cairoScope $ do
        let gridSize = ceiling (sqrt (fromIntegral (length items))) :: Integer
            margin = (w + h) * 0.05
            cellW = (w-2*margin) / fromIntegral gridSize
            cellH = (h-2*margin) / fromIntegral gridSize
        Cairo.translate margin margin
        for_ (zip items [(x, y) | x <- [0 .. gridSize - 1], y <- [0 .. gridSize - 1]]) $ \(item, (x, y)) -> cairoScope $ do
            Cairo.translate (fromIntegral x * cellW) (fromIntegral y * cellH)
            drawToSize (cellW, cellH) item
            pure ()
