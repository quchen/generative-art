module Geometry.Triangulate (
    triangulate
) where



import Data.List
import Data.Ord

import Geometry.Core
import Util



triangulate :: Polygon -> [Polygon]
triangulate = earClipping

isEar :: Polygon -> PolygonOrientation -> [Vec2] -> Bool
isEar candidate parentOrientation forbiddenPoints =
    not (any (\r -> pointInPolygon r candidate) forbiddenPoints)
    && parentOrientation == polygonOrientation candidate

-- | Ear-clipping algorithm – recursively find isolated triangles we can cut
-- off. Probably terrible performance for large polygons.
earClipping :: Polygon -> [Polygon]
earClipping parentPolygon = go parentPolygon
  where
    parentOrientation = polygonOrientation parentPolygon
    bestEar [] = bugError "No ears to cut off"
    bestEar xs = maximumBy (\(e1,_,_) (e2,_,_) -> comparing polygonArea e1 e2) xs
    onlyEars (candidate, eww, _) = isEar candidate parentOrientation eww

    go tri@(Polygon [_,_,_]) = [tri]
    go polygon@(Polygon (_:_:_:_)) = case (bestEar . filter onlyEars . triples) polygon of
        (ear, _, remainder) -> ear : go remainder
    go _other = error "oh no"

triples :: Polygon -> [(Polygon, [Vec2], Polygon)]
triples (Polygon ps)
  = map (\(p:x:q:rest) -> (Polygon [p,x,q], rest, Polygon (p:q:rest))) (rotations ps)
