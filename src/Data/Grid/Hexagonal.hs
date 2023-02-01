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
left      = goto L
right     = goto R
upLeft    = goto UL
upRight   = goto UR
downLeft  = goto DL
downRight = goto DR

goto :: Direction -> Grid a -> Maybe (Grid a)
goto direction (G.Grid h xs) =
    let h' = move direction 1 h
    in  G.Grid h' xs <$ M.lookup h' xs
