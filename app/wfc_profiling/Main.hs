{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)

import Draw
import Draw.Grid
import Geometry hiding (Grid)
import Geometry.Algorithms.WaveFunctionCollapse as Wfc

import System.Random.MWC (initialize)
import Control.Monad.ST (runST)
import qualified Data.MultiSet as M

gridSize :: Num a => a
gridSize = 20

main :: IO ()
main = render "out/wfc.png" 480 480 $
        drawGrid (480, 480) $
            fmap ((\[a] -> extractStencil a) . M.distinctElems) (last steps)
  where
    steps = runST $ do
        gen <- initialize (V.fromList [5])
        wfc (settingsFromGrid example) gridSize gridSize gen
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
