module Test.Geometry.Algorithms.WaveFunctionCollapse (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)

import Draw
import Geometry hiding (Grid)
import Geometry.Algorithms.WaveFunctionCollapse as Wfc

import Test.TastyAll

tests :: TestTree
tests = testGroup "WaveFunctionCollapse algorithm"
    [ testDrawGrid
    , testDuplicate
    ]

exampleGrid :: Grid XO
exampleGrid = fromListG
    [ [ X, O, X ]
    , [ X, X, O ]
    , [ X, O, O ]
    ]

testDrawGrid :: TestTree
testDrawGrid = testVisual "3x3 Grid" 240 240 "docs/grid" $ \(w, h) ->
    drawGrid drawXO (w, h) exampleGrid

testDuplicate :: TestTree
testDuplicate = testVisual "Duplicated grid" 240 240 "docs/grid_duplicate" $ \(w, h) ->
    drawGrid (drawGrid drawXO) (w, h) (duplicate exampleGrid)

zipperLength :: Zipper a -> Int
zipperLength = length . Wfc.toList

extents :: Grid a -> (Int, Int)
extents (Grid z) = (zipperLength (extractZ z), zipperLength z)

drawGrid :: ((Double, Double) -> a -> Render ()) -> (Double, Double) -> Grid a -> Cairo.Render ()
drawGrid renderCellContent (w, h) grid@(Grid zz) = cairoScope $ do
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
        renderCellContent (cellW, cellH) content
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

drawXO :: (Double, Double) -> XO -> Render ()
drawXO (w, h) xo = cairoScope $ do
    sketch (Polygon [Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h])
    setColor black
    strokePreserve
    setColor $ case xo of
        X -> white
        O -> rgb 1 0 0
    fill
