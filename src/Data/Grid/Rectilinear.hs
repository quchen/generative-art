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



type Grid = G.Grid (Int, Int)

left, right, up, down :: Grid a -> Maybe (Grid a)
left  g@(G.Grid (x, y) xs) = fmap (const g) (xs M.!? (x-1, y))
right g@(G.Grid (x, y) xs) = fmap (const g) (xs M.!? (x+1, y))
up    g@(G.Grid (x, y) xs) = fmap (const g) (xs M.!? (x, y-1))
down  g@(G.Grid (x, y) xs) = fmap (const g) (xs M.!? (x, y+1))
