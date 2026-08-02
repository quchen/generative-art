module Geometry.Algorithms.Culling (
    occlusionCulling
  , backfaceCulling
  , occlusionCullingProjection
) where

import Data.Foldable

import Geometry.Algorithms.Clipping
import Geometry.Core
import qualified Util.RTree as RT



-- * 2D

-- | Remove the parts of each projected polygon that are covered by previous
--   polygons earlier in the list (or closer to the viewer).
--
--   We process polygons from front to back, i.e. the first polygon in the list
--   is closest to the viewer. For each subsequent polygon we subtract
--   all polygons in front of it using 'differencePP'. This clips away the
--   parts of the polygon that are hidden behind closer geometry. The visible
--   fragments are then added to the set of occluders for subsequent polygons.
--
--   The occluders are stored in an 'RTree' keyed by their bounding box, so we
--   only need to run the expensive 'differencePP' against the occluders whose
--   bounding boxes actually overlap the current fragment.
occlusionCulling
    :: [Polygon] -- ^ Input list (sorted front to back)
    -> [Polygon]
occlusionCulling polys = go polys RT.empty
  where
    -- Process polygons from front to back, accumulating the projected
    -- polygons of all already-drawn (front) polygons as occluders in an
    -- 'RTree' for bounding-box-accelerated overlap queries.
    go :: [Polygon] -> RT.RTree Polygon -> [Polygon]
    go [] _occluders = []
    go (poly : rest) occluders =
        let visible = subtractOccluders poly occluders
            occluders' = foldl' (flip RT.insert) occluders visible
        in visible ++ go rest occluders'

-- | Subtract the occluder polygons from each of the given fragments, returning
--   the visible (non-occluded) fragments. Each fragment is kept only if it is
--   an 'Island' produced by the difference; 'Hole' fragments describe holes
--   punched into the subject and are not drawable on their own.
--
--   Only occluders whose bounding box intersects the bounding box of a fragment
--   are passed to 'differencePP', since non-overlapping polygons cannot
--   subtract from each other. The candidate occluders are found via the
--   'RTree' for an efficient bounding-box-accelerated overlap query.
subtractOccluders :: Polygon -> RT.RTree Polygon -> [Polygon]
subtractOccluders fragment occluders =
    let candidates = RT.intersect (boundingBox fragment) occluders
    in foldl' (\frags occluder -> concatMap (subtractOne occluder) frags) [fragment] candidates
  where
    subtractOne occluder frag =
        [ p | (p, Island) <- differencePP frag occluder, not (isEmptyPolygon p) ]
    -- A polygon with fewer than three corners has no interior and can be discarded.
    isEmptyPolygon :: Polygon -> Bool
    isEmptyPolygon (Polygon ps) = length ps < 3

-- * 3D

-- | Filter out faces that point away from the viewer.
--
-- Faces whose normal point in the same direction as the given normal survive,
-- faces whose normal point in the opposite direction are culled.
backfaceCulling
    :: Vec3 -- ^ Normal (pointing towards the viewer)
    -> [Triangle3]
    -> [Triangle3]
backfaceCulling n = filter (\t -> _triNormal t `dotProduct` n >= 0)

-- | 3D variant of occlusion culling that includes the projection
--
--   The viewer looks along @normal@ (the projection direction used in
--   'projection'). A triangle is closer to the viewer the larger its
--   projection along @normal@ is, i.e. the larger @v \`dotProduct\` normal@ is
--   for its vertices.
occlusionCullingProjection
    :: Vec3 -- ^ Normal (pointing towards the viewer)
    -> [Triangle3]
    -> [Polygon]
occlusionCullingProjection n = occlusionCulling . projection n . depthSorted n