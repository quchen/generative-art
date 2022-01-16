module Util.RTree (
  RTree()
, empty
, singleton

, insert
, delete

, union
, intersect
, lookupRange
, lookupContainsRange
, null

, fromList
, toList
) where

import Data.List ((\\))
import qualified Data.RTree as RT
import Prelude hiding (null)

import Geometry

newtype RTree a = Wrap { unwrap :: RT.RTree [a] }

instance Semigroup (RTree a) where
    (<>) = union

instance Monoid (RTree a) where
    mempty = empty

mbb :: HasBoundingBox a => a -> RT.MBB
mbb a = RT.mbb x1 y1 x2 y2
  where BoundingBox (Vec2 x1 y1) (Vec2 x2 y2) = boundingBox a

empty :: RTree a
empty = Wrap RT.empty

singleton :: HasBoundingBox a => a -> RTree a
singleton a = Wrap $ RT.singleton (mbb a) [a]

insert :: HasBoundingBox a => a -> RTree a -> RTree a
insert a = Wrap . RT.insertWith (++) (mbb a) [a] . unwrap

delete :: (Eq a, HasBoundingBox a) => a -> RTree a -> RTree a
delete a = Wrap . deleteOrUpdate . unwrap
  where
    bb = mbb a
    deleteOrUpdate tree = case RT.lookup bb tree of
        Just [b] | b == a -> RT.delete bb tree
        Just bs  | a `elem` bs -> RT.insert bb (bs \\ [a]) tree
        _otherwise -> tree

union :: RTree a -> RTree a -> RTree a
union (Wrap l) (Wrap r) = Wrap (RT.unionWith (++) l r)

intersect :: BoundingBox -> RTree a -> [a]
intersect bb = concat . RT.intersect (mbb bb) . unwrap

lookupRange :: BoundingBox -> RTree a -> [a]
lookupRange bb = concat . RT.lookupRange (mbb bb) . unwrap

lookupContainsRange :: BoundingBox -> RTree a -> [a]
lookupContainsRange bb = concat . RT.lookupContainsRange (mbb bb) . unwrap

null :: RTree a -> Bool
null = RT.null . unwrap

fromList :: HasBoundingBox a => [a] -> RTree a
fromList = Wrap . foldr (\a -> RT.insertWith (++) (mbb a) [a]) RT.empty

toList :: RTree a -> [a]
toList = concatMap snd . RT.toList . unwrap

