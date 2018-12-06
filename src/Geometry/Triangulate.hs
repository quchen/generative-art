module Geometry.Triangulate (
    triangulate
) where



import Geometry.Core



triangulate :: Polygon -> [Polygon]
triangulate = earClipping

-- | Ear-clipping algorithm – recursively find isolated triangles we can cut
-- off. Probably terrible performance for large polygons.
earClipping :: Polygon -> [Polygon]
earClipping poly = go [] poly
  where
    go saved (Polygon (p:x:q:xs))
      = let candidate = Polygon [p,x,q]
            accept = not (any (\r -> pointInPolygon r candidate) (saved ++ xs))
                     && polygonOrientation poly == polygonOrientation candidate
        in if accept
            then candidate : go saved (Polygon (p:q:xs)) -- Good ear, cut it off
            else go (p:saved) (Polygon (x:q:xs)) -- Bad ear, try next triangle
    go [] (Polygon _) = []
    go (s:aved) (Polygon xs) = go aved (Polygon (s:xs))
