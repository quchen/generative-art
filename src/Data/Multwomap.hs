module Data.Multwomap (
    Multwomap()
    , empty
    , null
    , size
    , union
    , insert
    , arbitraryKey
    , extract
) where



import           Data.Map (Map)
import qualified Data.Map as M
import           Prelude  hiding (null)

import Util



data OneTwo a = One a | Two a a deriving (Eq, Ord, Show)

-- | A multimap where each key can have at most two entries. Note that this is an
-- unsafe data structure: attempting to add a key twice will crash the program.
newtype Multwomap k v = Multwomap (Map k (OneTwo v)) deriving (Eq, Ord, Show)

empty :: Multwomap k v
empty = Multwomap M.empty

null :: Multwomap k v -> Bool
null (Multwomap mmap) = M.null mmap

size :: Multwomap k v -> Int
size (Multwomap mmap) = sum (M.map (\case One{} -> 1; Two{} -> 2) mmap)

insert :: Ord k => k -> v -> Multwomap k v -> Multwomap k v
insert k v (Multwomap mmap) = Multwomap (M.insertWith mergeOneTwo k (One v) mmap)

union :: Ord k => Multwomap k v -> Multwomap k v -> Multwomap k v
union (Multwomap mmap1) (Multwomap mmap2) = Multwomap (M.unionWith mergeOneTwo mmap1 mmap2)

mergeOneTwo :: OneTwo a -> OneTwo a -> OneTwo a
mergeOneTwo (One a) (One b) = Two a b
mergeOneTwo Two{} Two{} = bugError "Multwomap" "Overflow: both args already have two targets"
mergeOneTwo Two{} One{} = bugError "Multwomap" "Overflow: first arg already has two targets"
mergeOneTwo One{} Two{} = bugError "Multwomap" "Overflow: second arg already has two targets"

-- | Get an arbitrary key contained in the Multwomap, or 'Nothing' if it’s empty.
arbitraryKey :: Multwomap k v -> Maybe k
arbitraryKey (Multwomap mmap) = fmap fst (M.lookupMin mmap)

-- | Extract the value to the corresponding key, and return the Multwomap with the
-- key removed.
extract :: Ord k => k -> Multwomap k v -> Maybe (v, Multwomap k v)
extract k (Multwomap mmap) = case M.lookup k mmap of
    Nothing -> Nothing
    Just (One v) -> pure (v, Multwomap (M.delete k mmap))
    Just (Two v w) -> pure (v, Multwomap (M.insert k (One w) mmap))
