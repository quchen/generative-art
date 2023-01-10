{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Foldable
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)

import Draw
import Geometry hiding (Grid)
import Geometry.Algorithms.WaveFunctionCollapse as Wfc

import System.Random.MWC (create, initialize)
import Control.Monad.ST (runST)
import Data.Zipper (Zipper(..))
import qualified Data.Zipper as Z
import qualified Data.MultiSet as M

main :: IO ()
main = render "out/wfc.png" 480 480 $
        drawGrid (480, 480) $
            fmap ((\[a] -> extractStencil a) . M.distinctElems) (last steps)
  where
    steps = runST $ do
        gen <- initialize (V.fromList [5])
        wfc (settingsFromGrid example) 20 20 gen
    averageColor = average . fmap toColor

drawGrid :: DrawToSize a => (Double, Double) -> Grid a -> Cairo.Render ()
drawGrid (w, h) grid@(Grid zz) = cairoScope $ do
    Cairo.translate margin margin
    for_ (zip (Z.toList zz) [0..]) $ \(row, y) ->
        for_ (zip (Z.toList row) [0..]) $ \(cell, x) -> do
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
        let currentY = case zz of
                Zipper as _ _ -> length as
            currentX = case extract zz of
                Zipper as _ _ -> length as
        Cairo.translate (fromIntegral currentX * cellW) (fromIntegral currentY * cellH)
        Cairo.setLineWidth 2
        setColor $ rgb 0 0 1
        sketch (Polygon [Vec2 0 0, Vec2 cellW 0, Vec2 cellW cellH, Vec2 0 cellH])
        Cairo.stroke


class DrawToSize a where
    drawToSize :: (Double, Double) -> a -> Render ()

instance DrawToSize (Color Double) where
    drawToSize (w, h) color = cairoScope $ do
        sketch (Polygon [Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h])
        setColor black
        strokePreserve
        setColor color
        fill

instance DrawToSize XO where
    drawToSize (w, h) = drawToSize (w, h) . toColor

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
        for_ (zip items [(x, y) | y <- [0 .. gridSize - 1], x <- [0 .. gridSize - 1]]) $ \(item, (x, y)) -> cairoScope $ do
            Cairo.translate (fromIntegral x * cellW) (fromIntegral y * cellH)
            drawToSize (cellW, cellH) item
            pure ()

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

extractStencil :: Stencil3x3 a -> a 
extractStencil (Stencil3x3 _ _ _ _ e _ _ _ _) = e
