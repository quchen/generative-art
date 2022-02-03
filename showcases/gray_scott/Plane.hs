{-# LANGUAGE RecordWildCards #-}

module Plane where

import Control.Parallel.Strategies
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Prelude hiding (mod)
import qualified Prelude (mod)


mod :: Integral a => a -> a -> a
a `mod` b | a >= 0     = a `Prelude.mod` b
          | otherwise  = ((a `Prelude.mod` b) + b) `Prelude.mod` b

data Plane a = Plane
    { items     :: !(V.Vector a)
    , sizeX     :: {-# UNPACK #-} !Int
    , sizeY     :: {-# UNPACK #-} !Int
    }

planeToList :: V.Unbox a => Plane a -> [[a]]
planeToList Plane{..} =
    [ V.toList (V.slice (i * sizeX) sizeX items)
    | i <- [0 .. sizeY - 1]]

planeFromList :: V.Unbox a => [[a]] -> Plane a
planeFromList [] = error " Cannot construct empty Plane"
planeFromList xss = Plane {..}
  where
    sizeX = length (head xss)
    sizeY = length xss
    items = V.concat (V.fromList . checkLength <$> xss)
    checkLength xs = if length xs == sizeX then xs else error "Inconsistent row lengths"

at :: V.Unbox a => Plane a -> Int -> Int -> a
at Plane{..} x y = items V.! (x `mod` sizeX + (y `mod` sizeY) * sizeX)

foldNeighboursAt :: V.Unbox a => ((a, a, a, a, a, a, a, a, a) -> b) -> Int -> Int -> Plane a -> b
foldNeighboursAt f x y p =
    f ( at p (x-1) (y-1), at p x (y-1), at p (x+1) (y-1)
      , at p (x-1) y,     at p x y,     at p (x+1) y
      , at p (x-1) (y+1), at p x (y+1), at p (x+1) (y+1) )

mapNeighbours :: (V.Unbox a, V.Unbox b) => (Int -> Int -> (a, a, a, a, a, a, a, a, a) -> b) -> Plane a -> Plane b
mapNeighbours f plane@Plane{..} = plane { items = V.concat (fmap row [0..sizeY-1] `using` parList rdeepseq) }
  where
    row n = V.map cell (V.enumFromN (n * sizeX) sizeX)
    cell i =
        let x = i `mod` sizeX
            y = i `div` sizeX
        in  foldNeighboursAt (f x y) x y plane

mapPlane :: (V.Unbox a, V.Unbox b) => (a -> b) -> Plane a -> Plane b
mapPlane f plane@Plane{..} = plane { items = V.map f items }

modifyPlaneAt :: V.Unbox a => Int -> Int -> (a -> a) -> Plane a -> Plane a
modifyPlaneAt x y f plane@Plane{..} = plane { items = V.modify updateItem items }
  where updateItem v = MV.modify v f (x `mod` sizeX + (y `mod` sizeY) * sizeX)
