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

newtype RTree a = Wrap { unwrap :: RT.RTree (OneOrMany a) }

instance Semigroup (RTree a) where
    (<>) = union

instance Monoid (RTree a) where
    mempty = empty

data OneOrMany a
    = One {-# UNPACK #-} !a
    | Many {-# UNPACK #-} ![a]

merge :: OneOrMany a -> OneOrMany a -> OneOrMany a
merge (One a) (One b) = Many [a, b]
merge (One a) (Many b) = Many (a : b)
merge (Many a) (One b) = Many (b : a)
merge (Many a) (Many b) = Many (a ++ b)

oneOrMany :: [a] -> OneOrMany a
oneOrMany [a] = One a
oneOrMany as = Many as

flatten :: [OneOrMany a] -> [a]
flatten [] = []
flatten (One a : bs) = a : flatten bs
flatten (Many as : bs) = as ++ flatten bs

mbb :: HasBoundingBox a => a -> RT.MBB
mbb a = RT.mbb x1 y1 x2 y2
  where BoundingBox (Vec2 x1 y1) (Vec2 x2 y2) = boundingBox a

empty :: RTree a
empty = Wrap RT.empty

singleton :: HasBoundingBox a => a -> RTree a
singleton a = Wrap $ RT.singleton (mbb a) (One a)

insert :: HasBoundingBox a => a -> RTree a -> RTree a
insert a = Wrap . RT.insertWith merge (mbb a) (One a) . unwrap

delete :: (Eq a, HasBoundingBox a) => a -> RTree a -> RTree a
delete a = Wrap . deleteOrUpdate . unwrap
  where
    bb = mbb a
    deleteOrUpdate tree = case RT.lookup bb tree of
        Just (One b) | b == a -> RT.delete bb tree
        Just (Many bs) | a `elem` bs -> RT.insert bb (oneOrMany (bs \\ [a])) tree
        _otherwise -> tree

union :: RTree a -> RTree a -> RTree a
union (Wrap l) (Wrap r) = Wrap (RT.unionWith merge l r)

intersect :: BoundingBox -> RTree a -> [a]
intersect bb = flatten . RT.intersect (mbb bb) . unwrap

lookupRange :: BoundingBox -> RTree a -> [a]
lookupRange bb = flatten . RT.lookupRange (mbb bb) . unwrap

lookupContainsRange :: BoundingBox -> RTree a -> [a]
lookupContainsRange bb = flatten . RT.lookupContainsRange (mbb bb) . unwrap

null :: RTree a -> Bool
null = RT.null . unwrap

fromList :: HasBoundingBox a => [a] -> RTree a
fromList = Wrap . foldr (\a -> RT.insertWith merge (mbb a) (One a)) RT.empty

toList :: RTree a -> [a]
toList = flatten . fmap snd . RT.toList . unwrap

