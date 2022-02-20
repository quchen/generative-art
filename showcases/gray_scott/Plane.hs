{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
module Plane where

import Data.Array.Accelerate hiding (mod)
import qualified Data.Array.Accelerate as A
import Prelude ()
import qualified Prelude


mod :: Integral a => Exp a -> Exp a -> Exp a
a `mod` b = if a >= 0
    then a `A.mod` b
    else ((a `A.mod` b) + b) `A.mod` b

planeFromList :: Elt a => [[a]] -> Matrix a
planeFromList [] = error " Cannot construct empty Plane"
planeFromList xss = items
  where
    sizeX = Prelude.length (Prelude.head xss)
    sizeY = Prelude.length xss
    items = fromList (Z :. sizeY :. sizeX) (Prelude.concat xss)

mapNeighbours :: (Elt a, Elt b) => (Stencil3x3 a -> Exp b) -> Acc (Matrix a) -> Acc (Matrix b)
mapNeighbours f = stencil f clamp
