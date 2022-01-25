module Geometry.Triangulate (
    triangulate
) where



import Data.List
import Data.Ord

import Geometry.Core



-- | Split a polygon into a number of triangles.
triangulate :: Polygon -> [Polygon]
triangulate = triangulateByEarClipping

triangulateByEarClipping :: Polygon -> [Polygon]
triangulateByEarClipping polygon = case clipEar polygon of
    (ear, Nothing) -> [ear]
    (ear, Just rest) -> ear : triangulateByEarClipping rest

-- | A triangle is an ear if it has the same orientation as its parent, and does
-- not contain any other points of the parent.
isEar :: Polygon -> PolygonOrientation -> [Vec2] -> Bool
isEar candidate parentOrientation forbiddenPoints
  = and [ not (any (\r -> pointInPolygon r candidate) forbiddenPoints)
        , parentOrientation == polygonOrientation candidate
        ]

-- | Ear-clipping algorithm – find an isolated triangle we can cut off. Meant to
-- be iterated until the input is fully triangulated. Probably terrible
-- performance for large polygons.
clipEar :: Polygon -> (Polygon, Maybe Polygon)
clipEar parentPolygon = go parentPolygon
  where
    parentOrientation = polygonOrientation parentPolygon
    bestEar [] = bugError "No ears to cut off"
    bestEar xs = maximumBy (comparing (polygonArea . _earCandidate)) xs
    onlyEars c = isEar (_earCandidate c) parentOrientation (_untouchedVertices c)

    go :: Polygon -> (Polygon, Maybe Polygon)
    go lastEar@(Polygon [_,_,_]) = (lastEar, Nothing)
    go polygon@(Polygon (_:_:_:_)) =
        let bestClip = (bestEar . filter onlyEars . earClipCandidates) polygon
        in (_earCandidate bestClip, Just (_remainingPolygonIfClipped bestClip))
    go _other = bugError "Polygon with less than three vertices in triangulation"

data EarClipCandidate = EarClipCandidate
    { _earCandidate :: Polygon -- ^ Ear to clip off
    , _untouchedVertices :: [Vec2] -- ^ Polygon’s other vertices; for a good clip, these all have to be outside of the polygon
    , _remainingPolygonIfClipped :: Polygon -- ^ Should the ear be clipped, this is what remains
    }

earClipCandidates :: Polygon -> [EarClipCandidate]
earClipCandidates (Polygon ps) = do
    p:x:q:rest <- rotations ps
    pure EarClipCandidate {
        _earCandidate = Polygon [p,x,q]
        , _untouchedVertices = rest
        , _remainingPolygonIfClipped = Polygon (p:q:rest) }

-- | All rotations of a list.
--
-- prop> \n xs -> rotations xs !! n == rotate n xs
rotations :: [a] -> [[a]]
rotations = go []
  where
    go _ [] = []
    go xs (y:ys) = let xs' = xs ++ [y]
                       rotation = y:ys ++ xs
                   in rotation : go xs' ys
