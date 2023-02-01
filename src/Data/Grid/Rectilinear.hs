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
left  = goto (subtract 1, id)
right = goto ((+1), id)
up    = goto (id, subtract 1)
down  = goto (id, (+1))

goto :: (Int -> Int, Int -> Int) -> RectilinearGrid a -> Maybe (RectilinearGrid a)
goto (dx, dy) (Grid (x, y) xs) =
    let c' = (dx x, dy y)
    in  Grid c' xs <$ M.lookup c' xs

size :: RectilinearGrid a -> (Int, Int)
size (Grid _ xs) = bimap (+1) (+1) $ maximum (M.keys xs)
