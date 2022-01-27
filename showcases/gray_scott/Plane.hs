{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MonadComprehensions #-}

module Plane where

import Control.Comonad
import qualified Data.Vector as V
import Prelude hiding (mod)
import qualified Prelude (mod)


mod :: Integral a => a -> a -> a
a `mod` b | a >= 0     = a `Prelude.mod` b
          | otherwise  = ((a `Prelude.mod` b) + b) `Prelude.mod` b

--------------------------------------------------------------------------------
-- * Plane (two-dimensional 'Zipper')
--------------------------------------------------------------------------------

-- | A plane is a 'Zipper' of 'Zipper's. The outer layer zips through lines
-- (up\/down), the inner layer through columns (left\/right).
-- Like the 'Zipper', the 'Plane' has periodic boundary conditions.
data Plane a = Plane
    { items     :: {-# UNPACK #-} !(V.Vector a)
    , positionX :: {-# UNPACK #-} !Int
    , positionY :: {-# UNPACK #-} !Int
    , sizeX     :: {-# UNPACK #-} !Int
    , sizeY     :: {-# UNPACK #-} !Int
    } deriving (Functor)

moveLeft, moveRight, moveUp, moveDown :: Plane a -> Plane a
moveLeft  plane@Plane{..} = plane { positionX = (positionX - 1) `mod` sizeX }
moveRight plane@Plane{..} = plane { positionX = (positionX + 1) `mod` sizeX }
moveUp    plane@Plane{..} = plane { positionY = (positionY - 1) `mod` sizeY }
moveDown  plane@Plane{..} = plane { positionY = (positionY + 1) `mod` sizeY }


planeToList :: Plane a -> [[a]]
planeToList Plane{..} =
    [ V.toList (V.slice (i * sizeX) sizeX items)
    | i <- [0 .. sizeY - 1]]

planeFromList :: [[a]] -> Plane a
planeFromList [] = error " Cannot construct empty Plane"
planeFromList xss = Plane {..}
  where
    sizeX = length (head xss)
    sizeY = length xss
    positionX = 0
    positionY = 0
    items = V.concat (V.fromList . checkLength <$> xss)
    checkLength xs = if length xs == sizeX then xs else error "Inconsistent row lengths"

at :: Plane a -> Int -> Int -> a
at Plane{..} x y = items V.! ((x `mod` sizeX) * sizeX + y `mod` sizeY)

foldNeighboursAt :: ((a, a, a, a, a, a, a, a, a) -> b) -> Int -> Int -> Plane a -> b
foldNeighboursAt f x y p =
    f ( at p (x-1) (y-1), at p x (y-1), at p (x+1) (y-1)
      , at p (x-1) y,     at p x y,     at p (x+1) y
      , at p (x-1) (y+1), at p x (y+1), at p (x+1) (y+1) )

mapNeighbours :: ((a, a, a, a, a, a, a, a, a) -> b) -> Plane a -> Plane b
mapNeighbours f plane@Plane{..} = plane
    { items = fmap (\i -> foldNeighboursAt f (i `div` sizeX) (i `mod` sizeX) plane) (V.enumFromN 0 (sizeX * sizeY)) }
