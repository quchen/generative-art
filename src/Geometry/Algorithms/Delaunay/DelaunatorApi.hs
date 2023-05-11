{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | Nice API for Delaunator’s technical output.
module Geometry.Algorithms.Delaunay.DelaunatorApi (
      delaunayTriangulation
    , Triangulation(..)
    , D.TriangulationRaw
    , lloydRelaxation
    , Ray(..)
    , projectToViewport
) where



import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import qualified Data.Map                                as M
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

    , _voronoiCells :: Vector (Vec2, Polygon)
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

voronoiCell
    :: Vector Vec2 -- ^ Points
    -> D.TriangulationRaw
    -> Int -- ^ Index of an *incoming* edge towards the point in question
    -> Polygon
voronoiCell points delaunay e =
    let cellEdges = edgesAroundPoint delaunay e
        cellTriangles = map triangleOfEdge cellEdges
        vertices = map (\t -> circumcenter points delaunay t) cellTriangles
    in Polygon vertices

voronoiCells :: Vector Vec2 -> D.TriangulationRaw -> Vector (Vec2, Polygon)
voronoiCells points delaunay =
    let -- Index: Point ID to half-edge ID
        index = foldl' addToIndex M.empty [0..V.length (D._triangles delaunay)-1]
        addToIndex acc e =
            let endpoint = D._triangles delaunay ! D.nextHalfedge e
                hasSiblingHalfedge = D._halfedges delaunay ! e /= D.tEMPTY
                seen = M.member endpoint acc
            in if not seen || hasSiblingHalfedge
                then M.insert endpoint e acc
                else acc
    in V.catMaybes $ flip V.imap points $ \pIx pCoord ->
        let incoming = index M.! pIx
            polygon = voronoiCell points delaunay incoming
        in case polygon of
            Polygon (_1:_2:_3:_) -> Just (pCoord, polygon)
            _other -> Nothing

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
    -> Vector Vec2
lloydRelaxation omega = fmap newCenter . _voronoiCells . delaunayTriangulation
  where
    newCenter (old, cell) = old +. omega*.(old-.polygonCentroid cell)



-- ^ A ray is a line that extends to infinity on one side. Note that the direction
-- is a *direction* and not another point.
data Ray = Ray
    { _rayStart :: !Vec2 -- ^ Starting point
    , _rayDirection :: !Vec2 -- ^ Direction vector
    } deriving (Eq, Ord, Show)

-- | Where does the ray originating in the bounding box hit it from the inside first?
projectToViewport
    :: BoundingBox -- ^ Viewport
    -> Ray -- ^ Ray, originating within the bounding box
    -> Maybe Vec2 -- ^ Nothing if the ray never hits.
projectToViewport bbox ray = do
    let BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax) = bbox
        Ray {_rayStart=Vec2 x0 y0, _rayDirection=Vec2 vx vy} = ray
        t = 1/0

    (ty, yHit) <- case compare vy 0 of

        -- Direction points up (in screen coordinates), possibly hitting top
        LT | y0 <= yMin -> Nothing -- Starts above the bounding box, points away to infinity
           | c < t -> Just (c, Vec2 (x0 + c*vx) yMin)
           where c = (yMin-y0)/vy

        -- Direction points down (in screen coordinates), possibly hitting top
        GT | y0 >= yMax -> Nothing -- Starts below the bounding box, points away to infinity
           | c < t -> Just (c, Vec2 (x0 + c*vx) yMax)
           where c = (yMax-y0)/vy

        -- y search did not yield any result.
        -- Direction points straight left/right: pass decision on to x comparison.
        _otherwise -> Just (t, zero) -- zero is a dummy value, to be overwritten below.

    case compare vx 0 of

        -- Direction points left (in screen coordinates), possibly hitting left
        LT | x0 <= xMin -> Nothing  -- Starts on the left of the bounding box, points away to infinity
           | c < ty -> Just (Vec2 xMin (y0+c*vy))
           where c = (xMin-x0)/vx

        -- Direction points right (in screen coordinates), possibly hitting right
        GT | x0 >= xMax -> Nothing -- Starts on the right of the bounding box, points away to infinity
           | c < ty -> Just (Vec2 xMax (y0+c*vy))
           where c = (xMax-x0)/vx

        -- x did not yield better search, fall back to y’s result vector
        -- Direction points straight up/down: use whatever y calculation yielded
        _otherwise -> Just yHit
