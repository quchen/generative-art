{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Voronoi where

import Data.List (foldl', partition, (\\))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Random.MWC (GenIO, uniformR)
import System.Random.MWC.Distributions (normal)

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Geometry
import Data.Maybe (maybeToList)


newtype DelaunayTriangulation = Delaunay (S.Set (Triangle, Circle))

data Triangle = Triangle Vec2 Vec2 Vec2 deriving (Eq, Ord)
data Circle = Circle { center :: Vec2, radius :: Distance } deriving (Eq, Ord)

-- | Smart constructor for triangles.
--
-- Invariant: Orientation is always positive
triangle :: Vec2 -> Vec2 -> Vec2 -> Triangle
triangle p1 p2 p3 = case polygonOrientation (Polygon [p1, p2, p3]) of
    PolygonPositive -> Triangle p1 p2 p3
    PolygonNegative -> Triangle p1 p3 p2

toPolygon :: Triangle -> Polygon
toPolygon (Triangle p1 p2 p3) = Polygon [p1, p2, p3]

edges :: Triangle -> [Line]
edges (Triangle p1 p2 p3) = [Line p1 p2, Line p2 p3, Line p3 p1]

bowyerWatson :: [Vec2] -> DelaunayTriangulation
bowyerWatson ps = foldl' bowyerWatsonStep initialTriangulation ps
  where
    initialTriangle = triangle (Vec2 x1 y1) (Vec2 x1 (y1 + 2* (y2 - y1))) (Vec2 (x1 + 2*(x2-x1)) y1)
    Just initialCircumcircle = circumcircle initialTriangle
    initialTriangulation = Delaunay (S.singleton (initialTriangle, initialCircumcircle))
    BoundingBox (Vec2 x1 y1) (Vec2 x2 y2) = boundingBox ps

bowyerWatsonStep :: DelaunayTriangulation -> Vec2 -> DelaunayTriangulation
bowyerWatsonStep (Delaunay triangulation) newPoint = Delaunay (goodTriangles <> newTriangles)
  where
    (badTriangles, goodTriangles) = S.partition ((newPoint `inside`) . snd) triangulation
    outerPolygon = collectPolygon $ go (edges . fst =<< S.toList badTriangles) M.empty
      where
        go [] result = result
        go (Line p q : es) result
            -- Shared edges always have opposite direction, so they eliminate each other
            | q `M.member` result = go es (M.delete q result)
            | otherwise = go es (M.insert p q result)
    collectPolygon edgeGraph = let (p, _) = M.findMin edgeGraph in Polygon (go p edgeGraph)
      where
        go p remainingGraph
            | M.null remainingGraph = []
            | otherwise = let q = remainingGraph M.! p in p : go q (M.delete p remainingGraph)
    newTriangles = S.fromList
        [ (t, c)
        | Line p q <- polygonEdges outerPolygon
        , let t = triangle newPoint p q
        , c <- maybeToList (circumcircle t) ]

circumcircle :: Triangle -> Maybe Circle
circumcircle (Triangle p1 p2 p3) = case maybeCenter of
    Just center -> Just Circle { radius = norm (center -. p1), .. }
    Nothing -> Nothing
  where
    maybeCenter = case intersectionLL (Line p2 (midpoint p1 p3)) (Line p3 (midpoint p1 p2)) of
      IntersectionReal p -> Just p
      Collinear _ -> Nothing
      _ -> bugError "Circumcircle of triangle: Bad intersection"
    midpoint p q = (p +. q) /. 2

inside :: Vec2 -> Circle -> Bool
point `inside` Circle {..} = norm (center -. point) <= radius
