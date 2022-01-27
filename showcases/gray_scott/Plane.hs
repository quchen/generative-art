{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}

module Plane where

import Control.Comonad
import qualified Data.Vector.Unboxed as V
import Prelude hiding (mod)
import qualified Prelude (mod)


mod :: Integral a => a -> a -> a
a `mod` b | a >= 0     = a `Prelude.mod` b
          | otherwise  = ((a `Prelude.mod` b) + b) `Prelude.mod` b

data Plane a = Plane
    { items     :: {-# UNPACK #-} !(V.Vector a)
    , positionX :: {-# UNPACK #-} !Int
    , positionY :: {-# UNPACK #-} !Int
    , sizeX     :: {-# UNPACK #-} !Int
    , sizeY     :: {-# UNPACK #-} !Int
    }

moveLeft, moveRight, moveUp, moveDown :: Plane a -> Plane a
moveLeft  plane@Plane{..} = plane { positionX = (positionX - 1) `mod` sizeX }
moveRight plane@Plane{..} = plane { positionX = (positionX + 1) `mod` sizeX }
moveUp    plane@Plane{..} = plane { positionY = (positionY - 1) `mod` sizeY }
moveDown  plane@Plane{..} = plane { positionY = (positionY + 1) `mod` sizeY }


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
    positionX = 0
    positionY = 0
    items = V.concat (V.fromList . checkLength <$> xss)
    checkLength xs = if length xs == sizeX then xs else error "Inconsistent row lengths"

at :: V.Unbox a => Plane a -> Int -> Int -> a
at Plane{..} x y = items V.! ((x `mod` sizeX) * sizeX + y `mod` sizeY)

foldNeighboursAt :: V.Unbox a => ((a, a, a, a, a, a, a, a, a) -> b) -> Int -> Int -> Plane a -> b
foldNeighboursAt f x y p =
    f ( at p (x-1) (y-1), at p x (y-1), at p (x+1) (y-1)
      , at p (x-1) y,     at p x y,     at p (x+1) y
      , at p (x-1) (y+1), at p x (y+1), at p (x+1) (y+1) )

mapNeighbours :: (V.Unbox a, V.Unbox b) => ((a, a, a, a, a, a, a, a, a) -> b) -> Plane a -> Plane b
mapNeighbours f plane@Plane{..} = plane
    { items = V.map (\i -> foldNeighboursAt f (i `div` sizeX) (i `mod` sizeX) plane) (V.enumFromN 0 (sizeX * sizeY)) }

mapPlane :: (V.Unbox a, V.Unbox b) => (a -> b) -> Plane a -> Plane b
mapPlane f plane@Plane{..} = plane { items = V.map f items }
