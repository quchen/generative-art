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



data DelaunayTriangulation = DelaunayTriangulation
    { _dtTriangulation :: !(RT.RTree DelaunayTriangle)
    , _dtInitialPolygon :: !Polygon
    , _dtBounds :: {-# UNPACK #-} !BoundingBox
    }

data DelaunayTriangle = DT
    { _dtTriangle :: {-# UNPACK #-} !Triangle
    , _dtCircle :: {-# UNPACK #-} !Circle
    } deriving (Eq)

data Triangle = Triangle {-# UNPACK #-} !Vec2 {-# UNPACK #-} !Vec2 {-# UNPACK #-} !Vec2 deriving (Eq, Ord, Show)

instance HasBoundingBox Triangle where
    boundingBox = boundingBox . toPolygon

instance HasBoundingBox DelaunayTriangle where
    boundingBox (DT _ cc) = boundingBox cc

-- | Smart constructor for triangles.
--
-- The returned triangles always have 'PolygonPositive' orientation.
triangle :: Vec2 -> Vec2 -> Vec2 -> Triangle
triangle p1 p2 p3 = case polygonOrientation (Polygon [p1, p2, p3]) of
    PolygonPositive -> Triangle p1 p2 p3
    PolygonNegative -> Triangle p1 p3 p2

toPolygon :: Triangle -> Polygon
toPolygon (Triangle p1 p2 p3) = Polygon [p1, p2, p3]

edges :: Triangle -> [Line]
edges (Triangle p1 p2 p3) = [Line p1 p2, Line p2 p3, Line p3 p1]

getPolygons :: DelaunayTriangulation -> [Polygon]
getPolygons delaunay = deleteInitialPolygon $ toPolygon . _dtTriangle <$> RT.toList (_dtTriangulation delaunay)
  where
    deleteInitialPolygon = filter (not . hasCommonCorner (_dtInitialPolygon delaunay))
    hasCommonCorner (Polygon ps) (Polygon qs) = not . null $ ps `intersect` qs

-- | Calculate the Delaunay triangulation of a set of vertices using the Bowyer-Watson algorithm.
bowyerWatson :: HasBoundingBox bounds => bounds -> [Vec2] -> DelaunayTriangulation
bowyerWatson bounds = foldl' bowyerWatsonStep initialDelaunay
  where
    initialTriangles = [triangle v1 v2 v4, triangle v2 v3 v4]
    initialPolygon@(Polygon [v1, v2, v3, v4]) = transform (scaleAround (boundingBoxCenter bounds) 2) (boundingBoxPolygon bounds)
    initialDelaunay = DelaunayTriangulation
        { _dtTriangulation = RT.fromList (zipWith DT initialTriangles (fromJust . circumcircle <$> initialTriangles))
        , _dtBounds = boundingBox bounds
        , _dtInitialPolygon = initialPolygon
        }

-- | Add a new vertex to an existing Delaunay triangulation.
bowyerWatsonStep :: DelaunayTriangulation -> Vec2 -> DelaunayTriangulation
bowyerWatsonStep delaunay@DelaunayTriangulation{..} newPoint = delaunay { _dtTriangulation = foldr RT.insert goodTriangles newTriangles }
  where
    validNewPoint = if newPoint `insideBoundingBox` _dtBounds
        then newPoint
        else error "User error: Tried to add a point outside the bounding box"
    badTriangles = filter ((validNewPoint `inside`) . _dtCircle) (RT.lookupContainsRange (boundingBox validNewPoint) _dtTriangulation)
    goodTriangles = foldr RT.delete _dtTriangulation badTriangles
    outerPolygon = collectPolygon $ go (edges . _dtTriangle =<< badTriangles) M.empty
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

-- | Since the two concepts are closely related (by duality), we can convert a
-- 'DelaunayTriangulation' to a 'Voronoi' diagram rather easily.
toVoronoi :: DelaunayTriangulation -> Voronoi ()
toVoronoi delaunay = Voronoi
    { _voronoiBounds = _dtBounds delaunay
    , _voronoiCells =
        [ cell
        | (p, rays) <- M.toList (vertexGraph delaunay)
        , cell <- maybeToList (voronoiCell delaunay p rays)
        ]
    }

vertexGraph :: DelaunayTriangulation -> M.Map Vec2 (S.Set Vec2)
vertexGraph delaunay = go (_dtTriangle <$> RT.toList (_dtTriangulation delaunay)) M.empty
  where
    go [] = id
    go (Triangle a b c : rest) = go rest
        . M.alter (insertOrUpdate [b, c]) a
        . M.alter (insertOrUpdate [a, c]) b
        . M.alter (insertOrUpdate [a, b]) c
    insertOrUpdate ps Nothing = Just (S.fromList ps)
    insertOrUpdate ps (Just qs) = Just (S.union qs (S.fromList ps))

voronoiCell :: DelaunayTriangulation -> Vec2 -> S.Set Vec2 -> Maybe (VoronoiCell ())
voronoiCell delaunay p qs
    | p `elem` corners = Nothing
    | otherwise          = Just VoronoiCell { _voronoiRegion = polygonRestrictedToBounds, _voronoiSeed = p, _voronoiProps = () }
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
    polygonRestrictedToBounds = go (polygonEdges (boundingBoxPolygon (_dtBounds delaunay))) rawPolygon
      where
        go [] poly = poly
        go (l:ls) poly = let [clipped] = filter (p `pointInPolygon`) (cutPolygon l poly) in go ls clipped
    Polygon corners = _dtInitialPolygon delaunay

-- | Apply one iteration of Lloyd relaxation, moving the vertices of a
-- triangulation a bit. Applying this repeatedly results in a triangulation where
-- each triangle is approximately the same size.
lloydRelaxation :: DelaunayTriangulation -> DelaunayTriangulation
lloydRelaxation delaunay = bowyerWatson (_dtBounds delaunay) relaxedVertices
  where
    relaxedVertices = polygonCentroid . _voronoiRegion <$> _voronoiCells (toVoronoi delaunay)
