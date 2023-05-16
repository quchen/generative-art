-- | Nice API for Delaunator’s technical output.
module Geometry.Algorithms.Delaunay.DelaunatorApi (
      delaunayTriangulation
    , Triangulation(..)
    , VoronoiCell(..)
    , D.TriangulationRaw
    , lloydRelaxation

    -- * Experimental stuff, TODO remove
    , exteriorRays
    , ExtRays(..)
    , VoronoiPolygon(..)
    , projectToViewport
) where



import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import qualified Data.Map                                as M
import           Data.Vector                             (Vector, (!))
import qualified Data.Vector                             as V
import qualified Data.Vector.Mutable                     as VM
import qualified Geometry.Algorithms.Delaunay.Delaunator as D
import           Geometry.Core
import           Util



data Triangulation = Triangulation
    { _triangles :: Vector Polygon
    -- ^ All Delaunay triangles. Note that plotting these will have one line going
    -- back and one going forth between points not on the convex hull of the input.
    -- Use '_edges' if this is undesirable.

    , _edges :: [Line]
    -- ^ Each (undirected) edge of the Delaunay triangulation.

    , _voronoiCorners :: Vector Vec2
    -- ^ Corners of the Voronoi cells, useful for painting them in isolation. The
    -- entries are aligned with '_triangles', since the Voronoi corners are the
    -- circumcenters of the Delaunay triangles. You can group them correctly with
    -- 'V.zip'.

    , _voronoiEdges :: [Line]
    -- ^ Each (undirected) edge of the Voronoi diagram.

    , _extRays :: Vector ExtRays
    -- ^ TODO REMOVE

    , _voronoiCells :: Vector VoronoiCell
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
        (triPolygons, triCircumcenters) = triangles points raw
    in Triangulation
        { _triangles = triPolygons
        , _edges = edges points raw
        , _voronoiCorners = triCircumcenters
        , _voronoiEdges = voronoiEdges triCircumcenters raw
        , _voronoiCells = voronoiCells points triCircumcenters raw
        , _convexHull = convexHull' points raw
        , _raw = raw
        , _extRays = exteriorRays points raw
        }

instance NFData Triangulation where
    rnf (Triangulation a b c d e f g h) = rnf (a,b,c,d,e,f,g,h)

triangles :: Vector Vec2 -> D.TriangulationRaw -> (Vector Polygon, Vector Vec2)
triangles points triangulation =
    let triangleIxs = D._triangles triangulation
        corners = V.backpermute points triangleIxs
    in ( mapChunksOf3 (\x y z -> Polygon [x,y,z]) corners
       , mapChunksOf3 (\x y z -> D.circumcenter x y z) corners)

-- | @mapChunksOf3 f [a,b,c,  i,j,k,  p,q,r] = [f a b c,  f i j k,  f p q r]@
mapChunksOf3 :: (a -> a -> a -> b) -> Vector a -> Vector b
mapChunksOf3 f vec = V.create $ do
    let len = V.length vec `div` 3
    result <- VM.new len
    VM.iforM_ result $ \i _x -> do
        let x = vec ! (3*i+0)
            y = vec ! (3*i+1)
            z = vec ! (3*i+2)
        VM.write result i (f x y z)
    pure result

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

-- ^ Given a single edge, what’s the index of the start of the triangle?
triangleOfEdge :: Int -> Int
triangleOfEdge e = div e 3

voronoiEdges :: Vector Vec2 -> D.TriangulationRaw -> [Line]
voronoiEdges circumcenters triangulation = do
    let halfedgeIxs = D._halfedges triangulation
        numHalfedges = V.length halfedgeIxs

    e <- [0..numHalfedges-1]
    guard (e < halfedgeIxs!e)

    let t1 = triangleOfEdge e
        p1 = circumcenters ! t1

        t2 = triangleOfEdge (halfedgeIxs!e)
        p2 = circumcenters ! t2
    pure (Line p1 p2)

-- | All edges around a point. The point is specified by an incoming edge.
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

-- | A Voronoi Cell can either be an ordinary (finite) polygon,
-- or one that extends to infinity for boundary polygons.
data VoronoiPolygon
    = VoronoiFinite !Polygon -- ^ Ordinary polygon
    | VoronoiInfinite !Vec2 [Vec2] !Vec2
        -- ^ The polygon consists of a list of finite points, and extends to
        -- infinity at the beginning/end in the direction of the first/last
        -- argument. For example, the bottom/right quadrant (in screen coordinates)
        -- would be 'VornoiInfinite (Vec2 0 1) [Vec2 0 0] (Vec2 1 0)'.
    deriving (Eq, Ord, Show)

-- | Construct a single Voronoi polygon.
voronoiPolygon
    :: Vector Vec2 -- ^ Circumcenters
    -> D.TriangulationRaw
    -> Vector ExtRays -- ^ Exterior rays
    -> Int -- ^ Index of the point itself
    -> Int -- ^ Index of an incoming edge towards the point
    -> VoronoiPolygon
voronoiPolygon circumcenters delaunay extRays p e =
    let cellEdges = edgesAroundPoint delaunay e
        cellTriangles = map triangleOfEdge cellEdges
        vertices = map (circumcenters!) cellTriangles

    in case extRays!p of
        ExtRays dirIn dirOut -> VoronoiInfinite dirIn vertices dirOut
        NoExtRays -> VoronoiFinite (Polygon vertices)

instance NFData VoronoiPolygon where
    rnf VoronoiFinite{} = ()
    rnf (VoronoiInfinite _in ps _out) = rnf ps

-- | Map of point index to an incoming halfedge ID. Originates on the hull for hull
-- points possible, required for reconstructing edge polygons correctly.
inedges :: D.TriangulationRaw -> M.Map Int Int
inedges delaunay = V.ifoldl' addToIndex M.empty (D._triangles delaunay)
  where
    addToIndex acc e _t =
        let endpoint = D._triangles delaunay ! D.nextHalfedge e
            hasSiblingHalfedge = D._halfedges delaunay ! e /= D.tEMPTY
            seen = M.member endpoint acc
        in if not seen || not hasSiblingHalfedge
            then M.insert endpoint e acc
            else acc

-- | A Voronoi cell, consisting of a point and its Voronoi neighbourhood region.
data VoronoiCell = VoronoiCell
    { _voronoiCenter :: !Vec2
    , _voronoiPolygon :: !VoronoiPolygon
    } deriving (Eq, Ord, Show)

instance NFData VoronoiCell where
    rnf (VoronoiCell a b) = rnf (a,b)

voronoiCells :: Vector Vec2 -> Vector Vec2 -> D.TriangulationRaw -> Vector VoronoiCell
voronoiCells points circumcenters delaunay =
    let extRays = exteriorRays points delaunay
        index = inedges delaunay
    in flip V.imap points $ \pIx pCoord ->
        let incoming = index M.! pIx
            polygon = voronoiPolygon circumcenters delaunay extRays pIx incoming
        in VoronoiCell pCoord polygon

data ExtRays = NoExtRays | ExtRays !Vec2 !Vec2
    deriving (Eq, Ord, Show)

instance NFData ExtRays where
    rnf NoExtRays = ()
    rnf ExtRays{} = ()

-- | Each point on the Delaunay hull defines two rays:
--     1. The incoming edge (in hull traversal order), rotated by 90° outwards
--     2. The outgoing edge, rotated 90° outwards
--
-- We traverse the hull in order, and create a vector mapping point indices
-- to the rays originating from the incoming/outgoing edge.
--
-- The result vector has the structure /point -> (incoming, outgoing)/.
exteriorRays :: Vector Vec2 -> D.TriangulationRaw -> Vector ExtRays
exteriorRays points delaunay = runST $ do
    let hull = D._convexHull delaunay
    inRays <- VM.replicate (V.length points) Nothing
    outRays <- VM.replicate (V.length points) Nothing
    let recordRays = \pStart pEnd -> do
            let vecStart = points!pStart
                vecEnd = points!pEnd

                rayDir = rotate90 (vecEnd -. vecStart)

            -- Record as outgoing ray for pStart
            VM.write outRays pStart (Just rayDir)

            -- Record as incoming ray for pEnd
            VM.write inRays pEnd (Just rayDir)

    V.zipWithM_ recordRays hull (V.tail hull)
    _ <- recordRays (V.last hull) (V.head hull) -- zip omits the cyclic pair, so we do it manually

    (a, b) <- (,) <$> V.unsafeFreeze inRays <*> V.unsafeFreeze outRays
    pure (V.zipWith
        (\x y -> case (x,y) of
            (Just inDir, Just outDir) -> ExtRays inDir outDir
            (Nothing, Nothing) -> NoExtRays
            other -> bugError "exteriorRays" ("Bad external ray pair: " ++ show other)
        )
        a
        b)

-- | Rotate a 'Vec2' by 90°
rotate90 :: Vec2 -> Vec2
rotate90 (Vec2 x y) = Vec2 (-y) x

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
lloydRelaxation omega = fmap newCenter . abort . _voronoiCells . delaunayTriangulation
  where
    newCenter (old, cell) = old +. omega*.(old-.polygonCentroid cell)
    abort = error "Lloyd relaxation is disabled until infinite polygons work" -- TODO

-- ^ A ray is a line that extends to infinity on one side. Note that the direction
-- is a *direction* and not another point.
data Ray = Ray !Vec2 !Vec2 -- ^ Starting point and direction
    deriving (Eq, Ord, Show)

instance NFData Ray where
    rnf _ = () -- Already strict

-- | Where does the ray originating in the bounding box hit it from the inside first?
projectToViewport
    :: BoundingBox -- ^ Viewport
    -> Ray -- ^ Ray, originating within the bounding box
    -> Maybe Vec2 -- ^ Nothing if the ray never hits.
projectToViewport bbox ray = do
    let BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax) = bbox
        Ray (Vec2 x0 y0) (Vec2 vx vy) = ray
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
