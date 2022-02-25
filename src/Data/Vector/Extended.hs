module Data.Vector.Extended (
      fisherYatesShuffle
    , module Data.Vector
) where


import           Control.Monad.Primitive
import           Data.Foldable
import           Data.Vector
import qualified Data.Vector.Mutable     as VMut
import           System.Random.MWC



-- | Randomly permute a mutable 'Vector'.
--
-- You can use this via @'V.modify' 'fisherYatesShuffle'@ to permute an immutable vector.
fisherYatesShuffle
    :: PrimMonad f
    => Gen (PrimState f)
    -> MVector (PrimState f) a
    -> f ()
fisherYatesShuffle gen vec = do
    let n = VMut.length vec
    for_ [0..n-2] $ \i -> do
        j <- uniformRM (i, n-1) gen
        VMut.swap vec i j
