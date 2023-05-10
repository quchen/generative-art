-- | Nice API for Delaunator’s technical output.
module Geometry.Algorithms.Delaunay.DelaunatorApi (
      delaunayTriangulation
    , Triangulation(..)
    , D.TriangulationRaw
    , lloydRelaxation
) where



import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import qualified Data.Set                                as S
import           Data.Vector                             (Vector, (!))
import qualified Data.Vector                             as V
import qualified Geometry.Algorithms.Delaunay.Delaunator as D
import           Geometry.Core



data Triangulation = Triangulation
    { _triangles :: Vector Polygon
    -- ^ All Delaunay triangles. Note that plotting these will have one line going
    -- back and one going forth between points not on the convex hull of the input.
    -- Use '_edges' if this is undesirable.

    , _edges :: [Line]
    -- ^ Each (undirected) edge of the Delaunay triangulation.

    , _voronoiEdges :: [Line]
    -- ^ Each (undirected) edge of the Voronoi diagram.

    , _voronoiCells :: [(Vec2, Polygon)]
    -- ^ All Voronoi polygons

    , _convexHull :: Polygon
    -- ^ We get the convex hull for free out of the calculation. Equivalent to
    -- calling 'convexHull' on the input points.

    , _raw :: D.TriangulationRaw
    -- ^ Raw triangulation data. Import the internal module to access its constructors.

    } deriving (Eq, Ord, Show)

delaunayTriangulation :: Sequential vector => vector Vec2 -> Triangulation
delaunayTriangulation points' =
    let points = toVector points'
        raw = D.triangulate points
    in Triangulation
        { _triangles = triangles points raw
        , _edges = edges points raw
        , _voronoiEdges = voronoiEdges points raw
        , _voronoiCells = voronoiCells points raw
        , _convexHull = convexHull' points raw
        , _raw = raw
        }

instance NFData Triangulation where
    rnf (Triangulation a b c d e f) = rnf (a,b,c,d,e,f)

triangles :: Vector Vec2 -> D.TriangulationRaw -> Vector Polygon
triangles points triangulation =
    let triangleIxs = D._triangles triangulation
        numTriangles = V.length triangleIxs `div` 3
        corners = V.backpermute points triangleIxs
        -- I have a hunch this could be done with another single call to backpermute instead of generate/iterate…
    in V.generate numTriangles (constructTriangle corners)

constructTriangle :: Vector Vec2 -> Int -> Polygon
constructTriangle points i = Polygon [points!e | e <- edgesOfTriangle i]

pointsOfTriangle :: D.TriangulationRaw -> Int -> [Int]
pointsOfTriangle tri t = map (\e -> D._triangles tri ! e) (edgesOfTriangle t)

-- ^ Given a single edge, what’s the index of the start of the triangle?
triangleOfEdge :: Int -> Int
triangleOfEdge e = div e 3

edgesOfTriangle :: Int -> [Int]
edgesOfTriangle t = [3*t, 3*t+1, 3*t+2]

edges :: Vector Vec2 -> D.TriangulationRaw -> [Line]
edges points triangulation = do
    let triangleIxs = D._triangles triangulation
        halfedgeIxs = D._halfedges triangulation
        numHalfedges = V.length halfedgeIxs
    e <- [0..numHalfedges - 1]

    -- We arbitrarily select the larger of the two edges here. Note that this also
    -- covers the pair-less case, in which the opposite halfedge has index -1.
    guard (e > halfedgeIxs!e)
    let p = points!(triangleIxs!e)
        q = points!(triangleIxs!D.nextHalfedge e)
    pure (Line p q)

convexHull' :: Vector Vec2 -> D.TriangulationRaw -> Polygon
convexHull' points triangulation =
    let hullIxs = D._convexHull triangulation
        hull = V.backpermute points hullIxs
    in Polygon (toList hull)

voronoiEdges :: Vector Vec2 -> D.TriangulationRaw -> [Line]
voronoiEdges points triangulation = do
    let halfedgeIxs = D._halfedges triangulation
        numHalfedges = V.length halfedgeIxs

    e <- [0..numHalfedges-1]
    guard (e < halfedgeIxs!e)

    let t1 = triangleOfEdge e
        p1 = circumcenter points triangulation t1

        t2 = triangleOfEdge (halfedgeIxs!e)
        p2 = circumcenter points triangulation t2
    pure (Line p1 p2)

-- | Circumcenter of the i-th polygon.
circumcenter
    :: Vector Vec2 -- ^ Input points
    -> D.TriangulationRaw
    -> Int -- ^ i-th triangle
    -> Vec2
circumcenter points tri t =
    let [a,b,c] = pointsOfTriangle tri t
    in D.circumcenter (points!a) (points!b) (points!c)

edgesAroundPoint
    :: D.TriangulationRaw
    -> Int -- ^ Incoming (!) edge to the center point
    -> [Int]
edgesAroundPoint delaunay start = loop start
  where
    loop incoming =
        let outgoing = D.nextHalfedge incoming
            incoming' = D._halfedges delaunay ! outgoing
        in if incoming' /= D.tEMPTY && incoming' /= start
            then incoming : loop incoming'
            else [incoming]

atLeast3 :: [a] -> Bool
atLeast3 (_:_:_:_) = True
atLeast3 _ = False

voronoiCell
    :: Vector Vec2 -- ^ Points
    -> D.TriangulationRaw
    -> Int -- ^ Index of an *incoming* edge towards the point in question
    -> Maybe Polygon
voronoiCell points delaunay e = do
    let cellEdges = edgesAroundPoint delaunay e
        cellTriangles = map triangleOfEdge cellEdges
        vertices = map (\t -> circumcenter points delaunay t) cellTriangles
    guard (atLeast3 vertices)
    Just (Polygon vertices)

voronoiCells :: Vector Vec2 -> D.TriangulationRaw -> [(Vec2, Polygon)]
voronoiCells points delaunay = loop S.empty 0
  where
    loop _ e | e >= V.length (D._triangles delaunay) = []
    loop seen e =
        let p = D._triangles delaunay ! D.nextHalfedge e
            m'polygon = voronoiCell points delaunay e
            center = points!p
        in case m'polygon of
            Nothing -> loop (S.insert p seen) (e+1)
            Just polygon
                | S.member p seen -> loop seen (e+1)
                | otherwise -> (center, polygon) : loop (S.insert p seen) (e+1)

-- Relax the input points by moving them to(wards) their cell’s centroid, leading
-- to a uniform distribution of points. Works well when applied multiple times.
--
-- The parameter \(\omega\) controls how far the Voronoi cell center moves towards
-- the centroid.
-- [See here for a cool live visualization.](https://observablehq.com/@mbostock/lloyds-algorithm)
--
--   * \(0\) does not move the points at all.
--   * \(1\) moves the cell’s centers to the cell’s centroid (standard Lloyd).
--   * \(\sim 2\) overshoots the move towards the cell’s center, leading to faster convergence.
lloydRelaxation
    :: Sequential vector
    => Double -- ^ Convergence factor \(\omega\).
    -> vector Vec2
    -> [Vec2]
lloydRelaxation omega = fmap newCenter . _voronoiCells . delaunayTriangulation
  where
    newCenter (old, cell) = old +. omega*.(old-.polygonCentroid cell)
