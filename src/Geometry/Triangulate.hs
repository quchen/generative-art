module Geometry.Triangulate (
    triangulate
) where



import Geometry.Core



triangulate :: Polygon -> [Polygon]
triangulate = earClipping

-- | Ear-clipping algorithm – recursively find isolated triangles we can cut
-- off. Probably terrible performance for large polygons.
earClipping :: Polygon -> [Polygon]
earClipping polygon = go [] polygon
  where
    go skipped (Polygon (p:x:q:xs))
      = let candidate = Polygon [p,x,q]
            accept = not (any (\r -> pointInPolygon r candidate) xs)
                     && not (any (\r -> pointInPolygon r candidate) skipped)
                     && polygonOrientation polygon == polygonOrientation candidate
        in if accept
            then candidate : go skipped (Polygon (p:q:xs)) -- Good ear, cut it off
            else go (p:skipped) (Polygon (x:q:xs)) -- Bad ear, try next triangle

    -- Not enough vertices, restore skipped ones
    go skipped@(_:_) (Polygon xs) = go [] (Polygon (xs ++ reverse skipped))

    go [] (Polygon _) = []
