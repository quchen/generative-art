module Data.Grid.Hexagonal (
      module G
    , left
    , right
    , upLeft
    , upRight
    , downLeft
    , downRight

    , Comonad(..)
) where



import Control.Comonad
import qualified Data.Map.Strict as M

import Geometry.Coordinates.Hexagonal
import Data.Grid.Generic hiding (Grid)
import qualified Data.Grid.Generic as G



type Grid = G.Grid Hex

left, right, upLeft, upRight, downLeft, downRight :: Grid a -> Maybe (Grid a)
left      g@(G.Grid h xs) = fmap (const g) (xs M.!? move L  1 h)
right     g@(G.Grid h xs) = fmap (const g) (xs M.!? move R  1 h)
upLeft    g@(G.Grid h xs) = fmap (const g) (xs M.!? move UL 1 h)
upRight   g@(G.Grid h xs) = fmap (const g) (xs M.!? move UR 1 h)
downLeft  g@(G.Grid h xs) = fmap (const g) (xs M.!? move DL 1 h)
downRight g@(G.Grid h xs) = fmap (const g) (xs M.!? move DR 1 h)
