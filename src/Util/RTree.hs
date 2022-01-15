module Util.RTree (
  RT.RTree()
, RT.empty
, singleton

, insert
, delete

, RT.union
, intersect
, lookupRange
, lookupContainsRange
, RT.length
, RT.null
, RT.values

, fromList
, toList
) where

import qualified Data.RTree as RT
import Data.RTree (RTree)
import Geometry

mbb :: HasBoundingBox a => a -> RT.MBB
mbb a = RT.mbb x1 y1 x2 y2
  where BoundingBox (Vec2 x1 y1) (Vec2 x2 y2) = boundingBox a

singleton :: HasBoundingBox a => a -> RTree a
singleton a = RT.singleton (mbb a) a

insert :: HasBoundingBox a => a -> RTree a -> RTree a
insert a = RT.insert (mbb a) a

delete :: HasBoundingBox a => a -> RTree a -> RTree a
delete a = RT.delete (mbb a)

intersect :: BoundingBox -> RTree a -> [a]
intersect bb = RT.intersect (mbb bb)

lookupRange :: BoundingBox -> RTree a -> [a]
lookupRange bb = RT.lookupRange (mbb bb)

lookupContainsRange :: BoundingBox -> RTree a -> [a]
lookupContainsRange bb = RT.lookupContainsRange (mbb bb)

fromList :: HasBoundingBox a => [a] -> RTree a
fromList = RT.fromList . fmap (\a -> (mbb a, a))

toList :: RTree a -> [a]
toList = fmap snd . RT.toList

