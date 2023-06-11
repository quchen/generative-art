module Geometry.Algorithms.Triangulate (
    triangulate
) where



import Data.List
import Data.Ord

import Geometry.Core
import Util



-- $setup
-- >>> import Draw
-- >>> import qualified Graphics.Rendering.Cairo as C


-- | Split a polygon into a number of triangles.
--
-- Triangulations often make things easier to handle. For example, you may not know
-- the formula to calculate the area of a polygon. But if you know the area of a
-- triangle, then you can calculate the area by summing up the area of the
-- triangulated pieces.
--
-- <<docs/haddock/Geometry/Algorithms/Triangulate/triangulate.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Triangulate/triangulate.svg" 240 210 $ do
--     let polygon = Polygon [Vec2 10 74, Vec2 130 10, Vec2 140 143, Vec2 199 94, Vec2 232 175, Vec2 188 203, Vec2 35 133, Vec2 103 68]
--         triangles = triangulate polygon
--     C.setLineJoin C.LineJoinRound
--     for_ (zip [0..] triangles) $ \(i, triangle) -> cairoScope $ do
--         sketch triangle
--         setColor (mma i)
--         C.fillPreserve
--         C.stroke
--     cairoScope $ do
--         C.setLineWidth 2
--         setColor black
--         sketch polygon
--         C.stroke
-- :}
-- Generated file: size 4KB, crc32: 0xf8dda52c
triangulate :: Polygon -> [Polygon]
triangulate polygon = case clipEar polygon of
    (ear, Nothing) -> [ear]
    (ear, Just rest) -> ear : triangulate rest

isEar :: Polygon -> PolygonOrientation -> [Vec2] -> Bool
isEar candidate parentOrientation forbiddenPoints
  = not (any (\r -> pointInPolygon r candidate) forbiddenPoints)
    && parentOrientation == polygonOrientation candidate

-- | Ear-clipping algorithm – find an isolated triangle we can cut off. Meant to
-- be iterated until the input is fully triangulated. Probably terrible
-- performance for large polygons.
clipEar :: Polygon -> (Polygon, Maybe Polygon)
clipEar parentPolygon = go parentPolygon
  where
    parentOrientation = polygonOrientation parentPolygon
    bestEar [] = bugError "Triangulate.clipEar" "No ears to cut off"
    bestEar xs = maximumBy (\(e1,_,_) (e2,_,_) -> comparing polygonArea e1 e2) xs
    onlyEars (candidate, forbiddenPoints, _) = isEar candidate parentOrientation forbiddenPoints

    go lastEar@(Polygon [_,_,_]) = (lastEar, Nothing)
    go polygon@(Polygon (_:_:_:_)) = case (bestEar . filter onlyEars . triples) polygon of
        (ear, _, remainder) -> (ear, Just remainder)
    go _other = error "oh no"

triples :: Polygon -> [(Polygon, [Vec2], Polygon)]
triples (Polygon ps)
  = map (\(p:x:q:rest) -> (Polygon [p,x,q], rest, Polygon (p:q:rest))) (rotations ps)

-- | All rotations of a list.
rotations :: [a] -> [[a]]
rotations = go []
  where
    go _ [] = []
    go xs (y:ys) = let xs' = xs ++ [y]
                       rotation = y:ys ++ xs
                   in rotation : go xs' ys
