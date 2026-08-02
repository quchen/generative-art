module Util.RTree
    ( RTree
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
import qualified Data.R2Tree.Double as RT
import           Prelude    hiding (null)

import Geometry



-- | An 'RTree' allows querying objects overlapping with other objects efficiently.
newtype RTree a = Wrap { unwrap :: RT.R2Tree [a] }

-- | union
instance Semigroup (RTree a) where
    (<>) = union

-- | union/empty
instance Monoid (RTree a) where
    mempty = empty

mbr :: HasBoundingBox a => a -> RT.MBR
mbr a = RT.MBR x1 y1 x2 y2
  where BoundingBox (Vec2 x1 y1) (Vec2 x2 y2) = boundingBox a

-- | Empty 'RTree'.
empty :: RTree a
empty = Wrap RT.empty

-- | Single-element 'RTree'.
singleton :: HasBoundingBox a => a -> RTree a
singleton a = Wrap $ RT.singleton (mbr a) [a]

-- | Insert a single element into an 'RTree'.
insert :: HasBoundingBox a => a -> RTree a -> RTree a
insert a = Wrap . insertOrUpdate . unwrap
  where
    bx = mbr a
    insertOrUpdate tree =
        let acc _ bs acc' = bs : acc'
            existing = concat $ RT.foldrRangeWithKey (RT.equals bx) acc [] tree
        in RT.insert bx ([a] ++ existing) tree

-- | Delete an element from an 'RTree'. Does nothing if it’s not in there in the first place.
delete :: (Eq a, HasBoundingBox a) => a -> RTree a -> RTree a
delete a = Wrap . deleteOrUpdate . unwrap
  where
    bx = mbr a
    deleteOrUpdate tree =
        let acc _ bs acc' = bs : acc'
            existing = concat $ RT.foldrRangeWithKey (RT.equals bx) acc [] tree
        in case existing of
            [b] | b == a -> RT.delete bx tree
            bs  | a `elem` bs ->
                -- Remove the value by deleting the old entry and reinserting
                -- the remainder. (Same key may refer to any matching entry.)
                RT.insert bx (bs \\ [a]) (RT.delete bx tree)
            _otherwise -> tree

-- | Unions of two 'RTree's.
--
-- The @r-tree@ library does not support merge operations, so we fold the
-- right tree’s entries into the left one, combining values under equal 'MBR's
-- with @(++)@.
union :: RTree a -> RTree a -> RTree a
union (Wrap l) (Wrap r) = Wrap $ RT.foldrWithKey insertWithCombine l r
  where
    insertWithCombine bx bs acc =
        let existing = concat $ RT.foldrRangeWithKey (RT.equals bx) (\_ xs acc' -> xs : acc') [] acc
        in RT.insert bx (bs ++ existing) (RT.delete bx acc)

-- | All values intersecting the given 'BoundingBox'.
intersect :: BoundingBox -> RTree a -> [a]
intersect bb = concat . RT.foldrRangeWithKey (RT.intersects (mbr bb)) (\_ xs acc -> xs : acc) [] . unwrap

-- | Gather all elements fully contained in the given 'BoundingBox'.
--
-- > What’s inside this thing?
fullyContainedIn :: BoundingBox -> RTree a -> [a]
fullyContainedIn bb = concat . RT.foldrRangeWithKey (RT.containedBy (mbr bb)) (\_ xs acc -> xs : acc) [] . unwrap

-- | Gather all elements that fully contain the given 'BoundingBox'.
--
-- > What’s this thing contained in?
fullyContains :: BoundingBox -> RTree a -> [a]
fullyContains bb = concat . RT.foldrRangeWithKey (RT.contains (mbr bb)) (\_ xs acc -> xs : acc) [] . unwrap

fromList :: HasBoundingBox a => [a] -> RTree a
fromList = Wrap . foldr (\a -> RT.insert (mbr a) [a]) RT.empty

toList :: RTree a -> [a]
toList = concat . RT.foldr (:) [] . unwrap

instance Foldable RTree where
    foldMap f = foldMap f . toList
