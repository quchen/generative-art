{-# LANGUAGE RecordWildCards #-}

module Plane where

import Control.Parallel.Strategies
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Prelude hiding (mod)
import qualified Prelude (mod)


mod :: Integral a => a -> a -> a
a `mod` b | a >= 0     = a `Prelude.mod` b
          | otherwise  = ((a `Prelude.mod` b) + b) `Prelude.mod` b

-- Invariant (unchecked): blocks exactly cover the plane!
data Plane a = Plane
    { items     :: !(V.Vector (U.Vector a))
    , sizeX     :: {-# UNPACK #-} !Int  -- ^ width in pixels
    , sizeY     :: {-# UNPACK #-} !Int  -- ^ height in pixels
    , blocksX   :: {-# UNPACK #-} !Int  -- ^ number of blocks horizontally, needs to be divisor of sizeX
    , blocksY   :: {-# UNPACK #-} !Int  -- ^ number of blocks vertically, needs to be divisor of sizeY
    }

toPixels :: U.Unbox a => Plane a -> U.Vector a
toPixels plane@Plane{..} = U.concat
    [ U.slice (row * blockSizeX) blockSizeX (items V.! (bx + blocksX * by))
    | let blockSizeX = sizeX `div` blocksX
    , bx <- [0 .. blocksX]
    , row <- [0 .. sizeY `div` blocksY]
    , by <- [0 .. blocksY]
    ]

fromPixels :: U.Unbox a => Int -> Int -> Int -> Int -> (Int -> Int -> a) -> Plane a
fromPixels sizeX sizeY blockSizeX blockSizeY f = Plane {..}
  where
    blocksX = sizeX `div` blockSizeX
    blocksY = sizeY `div` blockSizeY
    items = V.fromList
        [ U.fromList
            [ f x y
            | y <- [by * blockSizeY .. (by+1) * blockSizeY - 1]
            , x <- [bx * blockSizeX .. (bx+1) * blockSizeX - 1]
            ]
        | bx <- [0..blocksX]
        , by <- [0..blocksY]
        ]

at :: U.Unbox a => Plane a -> Int -> Int -> a
at Plane{..} x y = items V.! (x `div` blocksX + (y `mod` blocksY) * blocksX) U.! (x `mod` (sizeX `div` blocksX) + (y `mod` (sizeY `div` blocksY)) * (sizeX `div` blocksX))

foldNeighboursAt :: U.Unbox a => ((a, a, a, a, a, a, a, a, a) -> b) -> Int -> Int -> Plane a -> b
foldNeighboursAt f x y p =
    f ( at p (x-1) (y-1), at p x (y-1), at p (x+1) (y-1)
      , at p (x-1) y,     at p x y,     at p (x+1) y
      , at p (x-1) (y+1), at p x (y+1), at p (x+1) (y+1) )

mapNeighbours :: (U.Unbox a, U.Unbox b) => (Int -> Int -> (a, a, a, a, a, a, a, a, a) -> b) -> Plane a -> Plane b
mapNeighbours f plane@Plane{..} = plane { items = V.fromList (U.map )


 (fmap row [0..sizeY-1] `using` parList rdeepseq) }
  where
    row n = U.map cell (U.enumFromN (n * sizeX) sizeX)
    cell i =
        let x = i `mod` sizeX
            y = i `div` sizeX
        in  foldNeighboursAt (f x y) x y plane
-- mapNeighbours :: (U.Unbox a, U.Unbox b) => (Int -> Int -> (a, a, a, a, a, a, a, a, a) -> b) -> Plane a -> Plane b
-- mapNeighbours f plane@Plane{..} = plane { items = U.concat (fmap row [0..sizeY-1] `using` parList rdeepseq) }
--   where
--     row n = U.map cell (U.enumFromN (n * sizeX) sizeX)
--     cell i =
--         let x = i `mod` sizeX
--             y = i `div` sizeX
--         in  foldNeighboursAt (f x y) x y plane

mapPlane :: (U.Unbox a, U.Unbox b) => (a -> b) -> Plane a -> Plane b
mapPlane f plane@Plane{..} = plane { items = V.map (U.map f) items }

--withBlocks :: U.Unbox a => Int -> (Plane a -> Plane a) -> Plane a -> Plane a
--withBlocks blockSize f plane@Plane{..} = plane { items = restorePlane (f <$> blocks) }
--  where
--    blocks =
--        [ Plane { sizeY = blockSize, posY = rowStart, .. }
--        | rowStart <- [0..sizeY `div` blockSize]
--        ]
--
--    restorePlane ps = U.concat
--        [ U.slice (pos * sizeX) (blockSize * sizeX) row
--        | Plane { posY = pos, items = row } <- ps
--        ]
