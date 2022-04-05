module Util.RTree (
    RTree()
    , empty
    , singleton

    , insert
    , delete

    , intersect
    , fullyContainedIn
    , fullyContains

    , fromList
) where



import           Data.List  ((\\))
import qualified Data.RTree as RT
import           Prelude    hiding (null)

import Geometry



-- | An 'RTree' allows querying objects overlapping with other objects efficiently.
newtype RTree a = Wrap { unwrap :: RT.RTree [a] }

-- | union
instance Semigroup (RTree a) where
    (<>) = union

-- | union/empty
instance Monoid (RTree a) where
    mempty = empty

mbb :: HasBoundingBox a => a -> RT.MBB
mbb a = RT.mbb x1 y1 x2 y2
  where BoundingBox (Vec2 x1 y1) (Vec2 x2 y2) = boundingBox a

-- | Empty 'RTree'.
empty :: RTree a
empty = Wrap RT.empty

-- | Single-element 'RTree'.
singleton :: HasBoundingBox a => a -> RTree a
singleton a = Wrap $ RT.singleton (mbb a) [a]

-- | Insert a single element into an 'RTree'.
insert :: HasBoundingBox a => a -> RTree a -> RTree a
insert a = Wrap . RT.insertWith (++) (mbb a) [a] . unwrap

-- | Delete an element from an 'RTree'. Does nothing if it’s not in there in the first place.
delete :: (Eq a, HasBoundingBox a) => a -> RTree a -> RTree a
delete a = Wrap . deleteOrUpdate . unwrap
  where
    bb = mbb a
    deleteOrUpdate tree = case RT.lookup bb tree of
        Just [b] | b == a -> RT.delete bb tree
        Just bs  | a `elem` bs -> RT.insert bb (bs \\ [a]) tree
        _otherwise -> tree

-- | Unions of two 'RTree's.
union :: RTree a -> RTree a -> RTree a
union (Wrap l) (Wrap r) = Wrap (RT.unionWith (++) l r)

-- | All values intersecting the given 'BoundingBox'.
intersect :: BoundingBox -> RTree a -> [a]
intersect bb = concat . RT.intersect (mbb bb) . unwrap

-- | Gather all elements fully contained in the given 'BoundingBox'.
--
-- > What’s inside this thing?
fullyContainedIn :: BoundingBox -> RTree a -> [a]
fullyContainedIn bb = concat . RT.lookupRange (mbb bb) . unwrap

-- | Gather all elements that fully contain the given 'BoundingBox'.
--
-- > What’s this thing contained in?
fullyContains :: BoundingBox -> RTree a -> [a]
fullyContains bb = concat . RT.lookupContainsRange (mbb bb) . unwrap

fromList :: HasBoundingBox a => [a] -> RTree a
fromList = Wrap . foldr (\a -> RT.insertWith (++) (mbb a) [a]) RT.empty

toList :: RTree a -> [a]
toList = concatMap snd . RT.toList . unwrap

instance Foldable RTree where
    foldMap f = foldMap f . toList
