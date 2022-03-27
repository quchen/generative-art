module Geometry.Algorithms.Delaunay.Internal (
    module Geometry.Algorithms.Delaunay.Internal
) where



import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set        as S
import qualified Util.RTree      as RT

import Geometry
import Geometry.Algorithms.Voronoi



data DelaunayTriangulation = Delaunay
    { triangulation :: !(RT.RTree DelaunayTriangle)
    , initialPolygon :: !Polygon
    , bounds :: {-# UNPACK #-} !BoundingBox
    }

data DelaunayTriangle = DT
    { dtTriangle :: {-# UNPACK #-} !Triangle
    , dtCircle :: {-# UNPACK #-} !Circle
    } deriving (Eq)

data Triangle = Triangle {-# UNPACK #-} !Vec2 {-# UNPACK #-} !Vec2 {-# UNPACK #-} !Vec2 deriving (Eq, Ord, Show)

instance HasBoundingBox Triangle where
    boundingBox = boundingBox . toPolygon

instance HasBoundingBox DelaunayTriangle where
    boundingBox (DT _ cc) = boundingBox cc

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

getPolygons :: DelaunayTriangulation -> [Polygon]
getPolygons Delaunay{..} = deleteInitialPolygon $ toPolygon . dtTriangle <$> RT.toList triangulation
  where
    deleteInitialPolygon = filter (not . hasCommonCorner initialPolygon)
    hasCommonCorner (Polygon ps) (Polygon qs) = not . null $ ps `intersect` qs

bowyerWatson :: BoundingBox -> [Vec2] -> DelaunayTriangulation
bowyerWatson bounds = foldl' bowyerWatsonStep initialDelaunay
  where
    initialTriangles = [triangle v1 v2 v4, triangle v2 v3 v4]
    initialPolygon@(Polygon [v1, v2, v3, v4]) = transform (scaleAround (center bounds) 2) (boundingBoxPolygon bounds)
      where center (BoundingBox p q) = (p +. q) /. 2
    initialDelaunay = Delaunay
        { triangulation = RT.fromList (zipWith DT initialTriangles (fromJust . circumcircle <$> initialTriangles))
        , .. }

bowyerWatsonStep :: DelaunayTriangulation -> Vec2 -> DelaunayTriangulation
bowyerWatsonStep delaunay@Delaunay{..} newPoint = delaunay { triangulation = foldr RT.insert goodTriangles newTriangles }
  where
    validNewPoint = if newPoint `insideBoundingBox` bounds
        then newPoint
        else error "User error: Tried to add a point outside the bounding box"
    badTriangles = filter ((validNewPoint `inside`) . dtCircle) (RT.lookupContainsRange (boundingBox validNewPoint) triangulation)
    goodTriangles = foldr RT.delete triangulation badTriangles
    outerPolygon = collectPolygon $ go (edges . dtTriangle =<< badTriangles) M.empty
      where
        go [] result = result
        go (Line p q : es) result
            -- Shared edges always have opposite direction, so they eliminate each other
            | maybe False (p `S.member`) (result M.!? q) = go es (M.adjust (S.delete p) q result)
            | otherwise = go es $ M.alter (\case
                Just qs -> Just (S.insert q qs)
                Nothing -> Just (S.singleton q))
                p result
    collectPolygon edgeGraph = let (p, _) = M.findMin edgeGraph in Polygon (go p edgeGraph)
      where
        go p remainingGraph
            | M.null remainingGraph = []
            | otherwise =
                let qs = remainingGraph M.! p
                    [q] = S.toList qs
                in p : go q (M.delete p remainingGraph)
    newTriangles =
        [ DT t c
        | Line p q <- polygonEdges outerPolygon
        , let t = triangle newPoint p q
        , c <- maybeToList (circumcircle t) ]

circumcircle :: Triangle -> Maybe Circle
circumcircle (Triangle p1 p2 p3) = case maybeCenter of
    Just center -> Just (Circle center (norm (center -. p1)))
    Nothing -> bugError ("Circumcircle of triangle: Bad intersection " ++ show intersectionOfBisectors)
  where
    intersectionOfBisectors = intersectionLL (perpendicularBisector (Line p1 p2)) (perpendicularBisector (Line p1 p3))
    maybeCenter = intersectionPoint intersectionOfBisectors

inside :: Vec2 -> Circle -> Bool
point `inside` Circle center radius = norm (center -. point) <= radius

toVoronoi :: DelaunayTriangulation -> Voronoi ()
toVoronoi delaunay@Delaunay{..} = Voronoi {..}
  where
    cells =
        [ cell
        | (p, rays) <- M.toList (vertexGraph delaunay)
        , cell <- maybeToList (voronoiCell delaunay p rays)
        ]

vertexGraph :: DelaunayTriangulation -> M.Map Vec2 (S.Set Vec2)
vertexGraph Delaunay{..} = go (dtTriangle <$> RT.toList triangulation) M.empty
  where
    go [] = id
    go (Triangle a b c : rest) = go rest
        . M.alter (insertOrUpdate [b, c]) a
        . M.alter (insertOrUpdate [a, c]) b
        . M.alter (insertOrUpdate [a, b]) c
    insertOrUpdate ps Nothing = Just (S.fromList ps)
    insertOrUpdate ps (Just qs) = Just (S.union qs (S.fromList ps))

voronoiCell :: DelaunayTriangulation -> Vec2 -> S.Set Vec2 -> Maybe (VoronoiCell ())
voronoiCell Delaunay{..} p qs
    | p `elem` corners = Nothing
    | otherwise          = Just VoronoiCell { region = polygonRestrictedToBounds, seed = p, props = () }
  where
    sortedRays = sortOn (getRad . angleOfLine) (Line p <$> S.toList qs)
    voronoiVertex l1 l2
        | Line _ q1 <- l1, Line _ q2 <- l2, q1 `elem` corners, q2 `elem` corners
          = intersectionPoint (intersectionLL (transform (rotateAround q1 (deg 90)) l1) (transform (rotateAround q2 (deg 90)) l2))
        | Line _ q <- l1, q `elem` corners
          = intersectionPoint (intersectionLL (transform (rotateAround q (deg 90)) l1) (perpendicularBisector l2))
        | Line _ q <- l2, q `elem` corners
          = intersectionPoint (intersectionLL (perpendicularBisector l1) (transform (rotateAround q (deg 90)) l2))
        | otherwise
          = intersectionPoint (intersectionLL (perpendicularBisector l1) (perpendicularBisector l2))
    rawPolygon = Polygon $ catMaybes (uncurry voronoiVertex <$> zip sortedRays (tail (cycle sortedRays)))
    polygonRestrictedToBounds = go (polygonEdges (boundingBoxPolygon bounds)) rawPolygon
      where
        go [] poly = poly
        go (l:ls) poly = let [clipped] = filter (p `pointInPolygon`) (cutPolygon l poly) in go ls clipped
    Polygon corners = initialPolygon

lloydRelaxation :: DelaunayTriangulation -> DelaunayTriangulation
lloydRelaxation delaunay@Delaunay{..} = bowyerWatson bounds relaxedVertices
  where
    relaxedVertices = polygonCentroid . region <$> cells (toVoronoi delaunay)
