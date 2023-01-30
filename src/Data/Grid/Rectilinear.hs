module Data.Grid.Rectilinear (
      module Data.Grid.Generic
    , RectilinearGrid
    , left
    , right
    , up
    , down

    , size

    , Comonad(..)
) where



import Control.Comonad
import Data.Bifunctor
import qualified Data.Map.Strict as M

import Data.Grid.Generic



type RectilinearGrid = Grid (Int, Int)

left, right, up, down :: RectilinearGrid a -> Maybe (RectilinearGrid a)
left  g@(Grid (x, y) xs) = fmap (const g) (xs M.!? (x-1, y))
right g@(Grid (x, y) xs) = fmap (const g) (xs M.!? (x+1, y))
up    g@(Grid (x, y) xs) = fmap (const g) (xs M.!? (x, y-1))
down  g@(Grid (x, y) xs) = fmap (const g) (xs M.!? (x, y+1))

size :: RectilinearGrid a -> (Int, Int)
size (Grid _ xs) = bimap (+1) (+1) $ maximum (M.keys xs)
