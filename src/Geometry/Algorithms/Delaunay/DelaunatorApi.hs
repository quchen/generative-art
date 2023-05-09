-- | Nice API for Delaunator’s technical output.
module Geometry.Algorithms.Delaunay.DelaunatorApi (triangles) where



import           Data.Foldable
import           Data.Vector                             (Vector)
import qualified Data.Vector                             as V
import qualified Geometry.Algorithms.Delaunay.Delaunator as D
import           Geometry.Core



triangles :: Vector Vec2 -> D.Triangulation -> [Polygon]
triangles points triangulation =
    let triangleIndices = D.__triangles triangulation
        !_ | mod (V.length triangleIndices) 3 /= 0 = error ("Delaunay triangulation broken: number of corners is not divisble by 3: " ++ show (V.length points))
           | otherwise = ()
        cornersInline = V.backpermute points triangleIndices
    in extractAllTriangles cornersInline

extractAllTriangles :: Vector Vec2 -> [Polygon]
extractAllTriangles points = case extractSingleTriangle points of
    Nothing -> []
    Just (triangle, rest) -> triangle : extractAllTriangles rest

extractSingleTriangle :: Vector Vec2 -> Maybe (Polygon, Vector Vec2)
extractSingleTriangle points
    | V.null points = Nothing
extractSingleTriangle points =
    let (corners, rest) = V.splitAt 3 points
        triangle = Polygon (toList corners)
    in Just (triangle, rest)
