{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
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
    { items     :: !(V.Vector (V.Vector a))
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
planeToList Plane{..} = V.toList <$> V.toList items

planeFromList :: [[a]] -> Plane a
planeFromList = \case
    []  -> error " Cannot construct empty Plane"
    xss -> Plane
        { items = V.fromList (V.fromList <$> xss)
        , positionX = 0
        , positionY = 0
        , sizeX = length (head xss)
        , sizeY = length xss
        }

extract :: Plane a -> a
extract Plane{..} = items V.! positionY V.! positionX

at :: Plane a -> Int -> Int -> a
at Plane{..} x y = items V.! (y `mod` sizeY) V.! (x `mod` sizeX)

foldNeighbours :: ((a, a, a, a, a, a, a, a, a) -> b) -> Plane a -> b
foldNeighbours f p = foldNeighboursAt f (positionX p) (positionY p) p

foldNeighboursAt :: ((a, a, a, a, a, a, a, a, a) -> b) -> Int -> Int -> Plane a -> b
foldNeighboursAt f x y p =
    f ( at p (x-1) (y-1), at p x (y-1), at p (x+1) (y-1)
      , at p (x-1) y,     at p x y,     at p (x+1) y
      , at p (x-1) (y+1), at p x (y+1), at p (x+1) (y+1) )

mapNeighbours :: ((a, a, a, a, a, a, a, a, a) -> b) -> Plane a -> Plane b
mapNeighbours f plane@Plane{..} = plane
    { items = fmap (\y -> fmap (\x -> foldNeighboursAt f x y plane) (V.enumFromN 0 sizeX)) (V.enumFromN 0 sizeY) }
