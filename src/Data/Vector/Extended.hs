module Data.Vector.Extended (
      fisherYatesShuffle
    , module Data.Vector
) where


import           Control.Monad.ST
import           Data.Foldable
import           Data.Vector
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VMut
import           System.Random.MWC



-- | Randomly permute a 'Vector'.
fisherYatesShuffle
    :: GenST s
    -> Vector a
    -> ST s (Vector a)
fisherYatesShuffle gen vec = do
    let n = V.length vec
    v <- V.thaw vec
    for_ [0..n-2] $ \i -> do
        j <- uniformRM (i, n-1) gen
        VMut.swap v i j
    V.unsafeFreeze v
