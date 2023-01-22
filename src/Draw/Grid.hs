module Draw.Grid where



import qualified Data.MultiSet as M
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo

import Draw
import Data.Grid
import Geometry



drawGrid :: DrawToSize a => (Double, Double) -> Grid a -> Cairo.Render ()
drawGrid (w, h) grid@(Grid l _ _ u zz) = cairoScope $ do
    Cairo.translate margin margin
    for_ (zip (V.toList zz) [0..]) $ \(row, y) ->
        for_ (zip (V.toList row) [0..]) $ \(cell, x) -> do
            drawCell x y cell
    highlightCurrent
  where
    (gridW, gridH) = size grid
    margin = (w + h) * 0.05
    cellW = (w-2*margin) / fromIntegral gridW
    cellH = (h-2*margin) / fromIntegral gridH
    drawCell x y content = cairoScope $ do
        Cairo.translate (x*cellW) (y*cellH)
        drawToSize (cellW, cellH) content
    highlightCurrent = do
        Cairo.translate (fromIntegral l * cellW) (fromIntegral u * cellH)
        Cairo.setLineWidth 1
        setColor $ mathematica97 3
        sketch (Polygon [Vec2 0 0, Vec2 cellW 0, Vec2 cellW cellH, Vec2 0 cellH])
        Cairo.stroke



class DrawToSize a where
    drawToSize :: (Double, Double) -> a -> Cairo.Render ()

instance DrawToSize a => DrawToSize (Grid a) where
    drawToSize = drawGrid

instance DrawToSize a => DrawToSize [a] where
    drawToSize (w, h) items = cairoScope $ do
        let gridSize = ceiling (sqrt (fromIntegral (length items))) :: Integer
            margin = (w + h) * 0.05
            cellW = (w-2*margin) / fromIntegral gridSize
            cellH = (h-2*margin) / fromIntegral gridSize
        Cairo.translate margin margin
        for_ (zip items [(x, y) | y <- [0 .. gridSize - 1], x <- [0 .. gridSize - 1]]) $ \(item, (x, y)) -> cairoScope $ do
            Cairo.translate (fromIntegral x * cellW) (fromIntegral y * cellH)
            drawToSize (cellW, cellH) item
            pure ()

instance DrawToSize a => DrawToSize (M.MultiSet a) where
    drawToSize (w, h) = drawToSize (w, h) . M.distinctElems
