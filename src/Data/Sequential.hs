module Data.Sequential (Sequential(..)) where



import           Data.Foldable
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Vector   (Vector)
import qualified Data.Vector   as V



-- | Data structures that are sequential, so that we can use them to build
-- input-polymorphic functions. This is quite handy, as a lot of functions require
-- special input, and it’s cumbersome to juggle 'V.fromList' and 'toList' all over
-- the place.
--
-- It’s a good idea to add the ideal type as part of the type signature,
--
-- @
-- -- This will work best with a vector input, but can convert everything else as well
-- doStuff :: 'Sequential' vector => vector a -> [a]
-- @
class Foldable f => Sequential f where
    toVector :: f a -> Vector a
    toSeq :: f a -> Seq a

instance Sequential [] where
    {-# INLINE toVector #-}
    toVector = V.fromList
    {-# INLINE toSeq #-}
    toSeq = Seq.fromList

instance Sequential Vector where
    {-# INLINE toVector #-}
    toVector = id
    {-# INLINE toSeq #-}
    toSeq = Seq.fromList . toList

instance Sequential Seq where
    {-# INLINE toVector #-}
    toVector = V.fromList . toList
    {-# INLINE toSeq #-}
    toSeq = id
