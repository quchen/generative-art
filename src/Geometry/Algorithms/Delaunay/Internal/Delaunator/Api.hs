-- | Nice API for Delaunator’s technical output.
module Geometry.Algorithms.Delaunay.Internal.Delaunator.Api where



import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import qualified Data.Map                     as M
import           Data.Vector                  (Vector, (!))
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Geometry.Algorithms.Clipping as Clipping
import           Geometry.Core
import           Util

import qualified Geometry.Algorithms.Delaunay.Internal.Delaunator.Raw as D

import           Debug.Trace
import           Draw
import           Geometry.Algorithms.Sampling
import           Graphics.Rendering.Cairo
import qualified System.Random.MWC            as MWC


-- $setup
-- >>> import           Draw
-- >>> import           Geometry.Algorithms.Sampling
-- >>> import           Geometry.Core                as G
-- >>> import           Graphics.Rendering.Cairo     as C
-- >>> import qualified Data.Vector                  as V
-- >>> import qualified System.Random.MWC            as MWC
-- >>>
-- >>> :{
-- >>> numPoints = 2^7
-- >>> numFindPoints = 8
-- >>> seed = [2]
-- >>> (width, height) = (600::Int, 400::Int)
-- >>> (points, findThesePoints) = runST $ do
-- >>>     gen <- MWC.initialize (V.fromList (map fromIntegral seed))
-- >>>     let margin = 100
-- >>>         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
-- >>>     ps <- uniformlyDistributedPoints gen bb numPoints
-- >>>     findUs <- uniformlyDistributedPoints gen bb numFindPoints
-- >>>     pure (ps, findUs)
-- >>> delaunay = delaunayTriangulation points
-- >>> :}


data Triangulation = Triangulation
    { _triangles :: Vector Polygon
    -- ^ All Delaunay triangles. Note that plotting these will have one line going
    -- back and one going forth between points not on the convex hull of the input.
    -- Use '_edges' if this is undesirable.
    --
    -- <<docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/triangles.svg>>
    --
    -- === __(image code)__
    -- >>> :{
    -- haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/triangles.svg" width height $ do
    --     setLineWidth 1
    --     let margin = 10
    --         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
    --     cairoScope $ do
    --         setColor (mathematica97 0)
    --         setDash [5,5] 0
    --         sketch (boundingBoxPolygon bb)
    --         stroke
    --     V.iforM_ (_triangles delaunay) $ \i triangle -> do
    --         setColor (mathematica97 i)
    --         sketch (growPolygon (-2) triangle)
    --         fill
    -- :}
    -- docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/triangles.svg

    , _edges :: [Line]
    -- ^ Each (undirected) edge of the Delaunay triangulation.
    --
    -- <<docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/edges.svg>>
    --
    -- === __(image code)__
    -- >>> :{
    -- haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/edges.svg" width height $ do
    --     setLineWidth 1
    --     let margin = 10
    --         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
    --     cairoScope $ do
    --         setColor (mathematica97 0)
    --         setDash [5,5] 0
    --         sketch (boundingBoxPolygon bb)
    --         stroke
    --     for_ (zip [0..] (_edges delaunay)) $ \(i, edge) -> do
    --         setColor (mathematica97 i)
    --         sketch edge
    --         stroke
    -- :}
    -- docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/edges.svg

    , _voronoiCorners :: Vector Vec2
    -- ^ Corners of the Voronoi cells, useful for painting them in isolation. The
    -- entries are aligned with '_triangles', since the Voronoi corners are the
    -- circumcenters of the Delaunay triangles. You can group them correctly with
    -- 'V.zip'.
    --
    -- <<docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_corners.svg>>
    --
    -- === __(image code)__
    -- >>> :{
    -- haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_corners.svg" width height $ do
    --     setLineWidth 1
    --     let margin = 10
    --         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
    --     cairoScope $ do
    --         setColor (mathematica97 0)
    --         setDash [5,5] 0
    --         sketch (boundingBoxPolygon bb)
    --         stroke
    --     for_ (clipEdgesToBox bb (_voronoiEdges delaunay)) $ \edge -> do
    --         setColor (mathematica97 0 `withOpacity` 0.2)
    --         sketch edge
    --         stroke
    --     V.iforM_ (_voronoiCorners delaunay) $ \i corner -> do
    --         setColor (mathematica97 i)
    --         sketch (Circle corner 2)
    --         fill
    -- :}
    -- docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_corners.svg

    , _voronoiEdges :: [Either Line Ray]
    -- ^ Each edge of the Voronoi diagram. The boundary edges extend to
    -- infinity, and are provided as 'Ray's.
    --
    -- 'clipEdgesToBox' conveniently handles the case of constraining
    -- this to a rectangular viewport.
    --
    -- <<docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_edges.svg>>
    --
    -- === __(image code)__
    -- >>> :{
    -- haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_edges.svg" width height $ do
    --     setLineWidth 1
    --     let margin = 10
    --         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
    --     cairoScope $ do
    --         setColor (mathematica97 0)
    --         setDash [5,5] 0
    --         sketch (boundingBoxPolygon bb)
    --         stroke
    --     for_ (zip [1..] (clipEdgesToBox bb (_voronoiEdges delaunay))) $ \(i, edge) -> do
    --         setColor (mathematica97 i)
    --         sketch edge
    --         stroke
    -- :}
    -- docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_edges.svg

    , _voronoiCells :: Vector VoronoiCell
    -- ^ All Voronoi polygons. The polygons at the hull can be infinite.
    --
    -- 'clipCellsToBox' conveniently handles the case of constraining
    -- this to a rectangular viewport.
    --
    -- <<docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_cells.svg>>
    --
    -- === __(image code)__
    -- >>> :{
    -- haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_cells.svg" width height $ do
    --     setLineWidth 1
    --     let margin = 10
    --         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
    --     cairoScope $ do
    --         setColor (mathematica97 0)
    --         setDash [5,5] 0
    --         sketch (boundingBoxPolygon bb)
    --         stroke
    --     V.iforM_ (clipCellsToBox bb (_voronoiCells delaunay)) $ \i polygon -> do
    --         setColor (mathematica97 i)
    --         sketch (growPolygon (-2) polygon)
    --         fill
    -- :}
    -- docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/voronoi_cells.svg

    , _convexHull :: Polygon
    -- ^ We get the convex hull for free out of the calculation. Equivalent to
    -- calling 'convexHull' on the input points.
    --
    -- <<docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/convex_hull.svg>>
    --
    -- === __(image code)__
    -- >>> :{
    -- haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/convex_hull.svg" width height $ do
    --     setLineWidth 1
    --     let margin = 10
    --         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
    --     cairoScope $ do
    --         setColor (mathematica97 0)
    --         setDash [5,5] 0
    --         sketch (boundingBoxPolygon bb)
    --         stroke
    --     sketch (_convexHull delaunay)
    --     setColor (mathematica97 1)
    --     stroke
    --     for_ points $ \p -> do
    --         sketch (Circle p 2)
    --         setColor (mathematica97 3)
    --         fill
    -- :}
    -- docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/convex_hull.svg

    , _findClosestInputPoint :: Vec2 -> Int -> Int
    -- ^ Find the index of the closest input point.
    --
    -- @'_findClosestInputPoint' needle start@ returns the index @i@ of the closest input
    -- point to @needle@, starting the search at @start@. @start=0@ searches the
    -- entire input. @points!i@ is the closest point’s position.
    --
    -- <<docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/find_triangle.svg>>
    --
    -- === __(image code)__
    -- >>> :{
    -- haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/find_triangle.svg" width height $ do
    --     setLineWidth 1
    --     let margin = 10
    --         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
    --     cairoScope $ do
    --         setColor (mathematica97 0)
    --         setDash [5,5] 0
    --         sketch (boundingBoxPolygon bb)
    --         stroke
    --     let foundTriangleIndices = [(p, _findClosestInputPoint delaunay p 0) | p <- toList findThesePoints]
    --     for_ (zip [0..] foundTriangleIndices) $ \(i, (point, t)) -> do
    --         let triangle = _triangles delaunay ! t
    --         sketch triangle >> setColor (mathematica97 i) >> fill
    --         sketch (Circle point 2) >> fill
    --         sketch (Circle point 2) >> setColor black >> stroke
    --     forM_ (clipEdgesToBox bb (_voronoiEdges delaunay)) $ \edge -> do
    --         setColor black
    --         sketch edge
    --         stroke
    -- :}
    -- docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/find_triangle.svg

    , _raw :: D.TriangulationRaw
    -- ^ Raw triangulation data. Import the internal module to access its constructors.
    }

-- | Create a 'Triangulation' from a set of points. See 'Triangulation' for further
-- details.
delaunayTriangulation :: Sequential vector => vector Vec2 -> Triangulation
delaunayTriangulation points' =
    let points = toVector points'
        raw = D.triangulate points
        (triPolygons, triCircumcenters) = triangles points raw
        extRays = exteriorRays points raw
        inedges = bulidInedgesLookup raw
        findTriangle needle i0 = findClosestInputPointIndex points inedges raw needle i0
    in Triangulation
        { _triangles = triPolygons
        , _edges = edges points raw
        , _findClosestInputPoint = findTriangle
        , _voronoiCorners = triCircumcenters
        , _voronoiEdges = voronoiEdges triCircumcenters raw extRays
        , _voronoiCells = voronoiCells points triCircumcenters inedges raw
        , _convexHull = convexHullViaDelaunay points raw
        , _raw = raw
        }

instance NFData Triangulation where
    rnf Triangulation
        { _triangles = x1
        , _edges = x2
        , _findClosestInputPoint = _
        , _voronoiCorners = x3
        , _voronoiEdges = x4
        , _voronoiCells = x5
        , _convexHull = x6
        , _raw = x7
        } = rnf (x1, x2, x3, x4, x5, x6, x7)

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
        halfedges = D._halfedges triangulation
        numHalfedges = V.length halfedges
    e <- [0..numHalfedges - 1]

    -- We arbitrarily select the larger of the two edges here. Note that this also
    -- covers the pair-less case, in which the opposite halfedge has index -1.
    guard (e > halfedges!e)
    let p = points!(triangleIxs!e)
        q = points!(triangleIxs!D.nextHalfedge e)
    pure (Line p q)

convexHullViaDelaunay :: Vector Vec2 -> D.TriangulationRaw -> Polygon
convexHullViaDelaunay points triangulation =
    let hull = V.backpermute points (D._convexHull triangulation)
    in Polygon (toList hull)

-- ^ Given a single edge, what’s the index of the start of the triangle?
triangleOfEdge :: Int -> Int
triangleOfEdge e = div e 3

voronoiEdges :: Vector Vec2 -> D.TriangulationRaw -> Vector ExtRays -> [Either Line Ray]
voronoiEdges circumcenters triangulation extRays = do
    let halfedges = D._halfedges triangulation
        numHalfedges = V.length halfedges

    -- guard (e < e')
    --     -- Note: halfedges!e can be -1 (if there is no partner edge).
    --     -- This is implicitly handled by the inequality here as well.
    e <- [0..numHalfedges-1]
    let e' = halfedges!e
        pStart = circumcenters ! triangleOfEdge e
    if
        | e' == D.tEMPTY ->
            let ExtRays _inDir outDir = extRays ! (D._triangles triangulation ! e)
                -- I don’t know why it’s outDir and not inDir, but I’m quite happy
                -- it’s consistent in my tests. I would have expected more random
                -- behavior. Lucky me!
            in [Right (Ray pStart outDir)]
        | e < e' ->
            let pEnd = circumcenters ! triangleOfEdge e'
            in [Left (Line pStart pEnd)]
        | otherwise -> []

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
bulidInedgesLookup :: D.TriangulationRaw -> M.Map Int Int
bulidInedgesLookup delaunay = V.ifoldl' addToIndex M.empty (D._triangles delaunay)
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

voronoiCells :: Vector Vec2 -> Vector Vec2 -> M.Map Int Int -> D.TriangulationRaw -> Vector VoronoiCell
voronoiCells points circumcenters inedges delaunay =
    let extRays = exteriorRays points delaunay
    in flip V.imap points $ \pIx pCoord ->
        let incoming = inedges M.! pIx
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

-- | Relax the input points by moving them to(wards) their cell’s centroid, leading
-- to a uniform distribution of points. Works well when applied multiple times.
--
-- The parameter \(\omega\) controls how far the Voronoi cell center moves towards
-- the centroid.
-- [See here for a cool live visualization.](https://observablehq.com/@mbostock/lloyds-algorithm)
--
--   * \(0\) does not move the points at all.
--   * \(1\) moves the cell’s centers to the cell’s centroid (standard Lloyd).
--   * \(\sim 2\) overshoots the move towards the cell’s center, leading to faster convergence.
--   * \(<0\) values yield wonky-but-interesting behavior! \(\ddot\smile\)
--
-- <<docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/lloyd_relaxation.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/lloyd_relaxation.svg" width height $ do
--     setLineWidth 1
--     let margin = 10
--         bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
--     cairoScope $ do
--         setColor (mathematica97 0)
--         setDash [5,5] 0
--         sketch (boundingBoxPolygon bb)
--         stroke
--     let points' = iterate (lloydRelaxation bb 1) points !! 5
--         delaunay' = delaunayTriangulation points'
--     V.iforM_ (clipCellsToBox bb (_voronoiCells delaunay')) $ \i polygon -> do
--         setColor (mathematica97 i)
--         sketch (growPolygon (-2) polygon)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/lloyd_relaxation.svg
lloydRelaxation
    :: (HasBoundingBox boundingBox, Sequential vector)
    => boundingBox
    -> Double -- ^ Convergence factor \(\omega\).
    -> vector Vec2
    -> Vector Vec2
lloydRelaxation bb omega = relax . _voronoiCells . delaunayTriangulation
  where
    newCenter old cell = old +. omega*.(polygonCentroid cell-.old)

    relax :: Vector VoronoiCell -> Vector Vec2
    relax cells = V.zipWith
        (\(VoronoiCell center _) polygon -> newCenter center polygon)
        cells
        (clipCellsToBox (boundingBox bb) cells)

-- ^ A ray is a line that extends to infinity on one side. Note that the direction
-- is a *direction* and not another point.
data Ray = Ray !Vec2 !Vec2 -- ^ Starting point and direction
    deriving (Eq, Ord, Show)

instance NFData Ray where
    rnf _ = () -- Already strict

-- | Convert a 'Ray' to a 'Line', cutting it off when it hits the 'BoundingBox'.
clipRay
    :: BoundingBox
    -> Ray
    -> Maybe Line -- ^ Nothing if the ray does not hit the bounding box.
clipRay bb ray = Clipping.cohenSutherland bb (comicallyLengthen bb ray)

-- | Cut off all 'Ray's to end at the provided 'BoundingBox'. Convenient to take
-- the result of '_voronoiEdges' and clip it to a rectangular viewport.
clipEdgesToBox
    :: HasBoundingBox boundingBox
    => boundingBox
    -> [Either Line Ray]
    -> [Line]
clipEdgesToBox bb' segments = do
    let bb = boundingBox bb'
    segment <- segments
    case segment of
        Left line -> case Clipping.cohenSutherland bb line of
            Just line' -> [line']
            Nothing -> []
        Right ray -> case clipRay bb ray of
            Just line -> [line]
            Nothing -> []

-- | Cut off all infinite 'VoronoiCells' with the provided 'BoundingBox'. Convenient to take
-- the result of '_voronoiCells' and clip it to a rectangular viewport.
--
-- This function yields incorrect results when the angle between the directions is
-- too large, because it simply comically enlarges the »infinite« directions to
-- finite size, closes the then finite polygon, and clips the resulting polygon.
-- Since Voronoi cells don’t produce such wide angels for even small point sizes,
-- this is a worthwhile tradeoff. The issue can probably be hacked around by adding
-- another point for all corners enclosed by the direction vectors.
clipCellsToBox
    :: HasBoundingBox boundingBox
    => boundingBox
    -> Vector VoronoiCell
    -> Vector Polygon
clipCellsToBox bb' = V.map $ \(VoronoiCell _center vPoly) -> case vPoly of
    VoronoiFinite polygon -> Clipping.sutherlandHodgman polygon  viewport
    VoronoiInfinite dirIn vertices dirOut ->
        let comicallyLargePolygon = Polygon ([looongIn] ++ vertices ++ [looongOut])
            Line _ looongIn = comicallyLengthen bb (Ray (head vertices) dirIn)
            Line _ looongOut = comicallyLengthen bb (Ray (last vertices) dirOut)
        in Clipping.sutherlandHodgman comicallyLargePolygon viewport
  where
    bb = boundingBox bb'
    viewport = boundingBoxPolygon bb

-- | Create a stupidly long line out of a 'Ray' so that it definitely spans well
-- over the bounding box.
comicallyLengthen :: BoundingBox -> Ray -> Line
comicallyLengthen bb (Ray start dir) =
    let BoundingBox bbMin bbMax = bb
        boundingBoxDiagonalNormSquare = normSquare (bbMin -. bbMax)
        dirNormSquare = normSquare dir
        end = start +. (max 1 boundingBoxDiagonalNormSquare / max 1 dirNormSquare) *. dir
    in Line start end

-- | Find the input point closest to the needle. Search starts at specified point i.
findClosestInputPointIndex
    :: Vector Vec2   -- ^ Input points
    -> M.Map Int Int -- ^ Incoming edges table
    -> D.TriangulationRaw
    -> Vec2          -- ^ Needle: which input point is closest to this?
    -> Int           -- ^ Start the search at this index. 0 searches from the beginning.
    -> Int           -- ^ Index of the closest point
findClosestInputPointIndex points inedges tri needle i0 = loopFind i0
  where
    loopFind i =
        let c' = step i
        in if c' >= 0 && c' /= i && c' /= i0
            then loopFind c'
            else c'

    step j | M.notMember j inedges = error "TODO what is this case for"
    step j =
        let c = j
            dc = normSquare (needle -. points!j)
            e0 = inedges M.! j
            e = e0
        in loopStep c dc e0 e

    loopStep
        :: Int    -- c: Start of search (candidate)
        -> Double -- dc: Distance² from candidate to needle
        -> Int    -- e0: inedge at point i
        -> Int    -- e:  ???
        -> Int    -- Better candidate after the step
    loopStep c dc e0 e = traceShow ("   ", "loopStep", "c", c, "dc", dc, "e0", e0, "e", e) $
        let t = D._triangles tri ! e
            dt = normSquare (needle -. points!t)
            (dc', c') | dt < dc   = (dt, t)
                        | otherwise = (dc, c)
            e' = D._halfedges tri ! D.nextHalfedge e
            -- !_ = traceShow ("   ", "Sanity check: ", D._triangles tri ! D.nextHalfedge e == i) () -- TODO CLEANUP
        in if e' == D.tEMPTY
            then -- The next edge has no partner: we’re on the hull
                let e'' = error "TODO hull handling" -- JS: e = hull[(_hullIndex[i] + 1) % hull.length];
                in if e'' /= t && normSquare (needle -. points!e) < dc'
                    then e'' -- JS: return e
                    else c' -- JS: break
            else -- We’re not on the hull
                if e' /= e0
                    then loopStep c' dc' e0 e'
                    else c'

testi :: t -> IO ()
testi _ = go
  where
    numPoints = 2^6
    numFindPoints = 10
    seed = [3]
    (width, height) = (600::Int, 400::Int)
    (points, findThesePoints) = runST $ do
        gen <- MWC.initialize (V.fromList (map fromIntegral seed))
        let margin = 100
            bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
        ps <- uniformlyDistributedPoints gen bb numPoints
        let margin' = 150
            bb' = boundingBox [Vec2 margin' margin', Vec2 (fromIntegral width - margin') (fromIntegral height - margin')]
        findUs <- uniformlyDistributedPoints gen bb' numFindPoints
        pure (ps, findUs)
    delaunay = delaunayTriangulation points
    go = haddockRender "Geometry/Algorithms/Delaunay/Internal/Delaunator/Api/find_triangle.svg" width height $ do
        setLineWidth 1
        let margin = 10
            bb = boundingBox [Vec2 margin margin, Vec2 (fromIntegral width - margin) (fromIntegral height - margin)]
        cairoScope $ do
            setColor (mathematica97 0)
            setDash [5,5] 0
            sketch (boundingBoxPolygon bb)
            stroke
        for_ (_edges delaunay) $ \edge -> do
            setColor (black `withOpacity` 0.5)
            sketch edge
            stroke
        let foundTriangleIndices = [(p, _findClosestInputPoint delaunay p 0) | p <- toList findThesePoints]
        for_ (zip [0..] foundTriangleIndices) $ \(i, (needle, p)) -> do
            let closest = points ! p
            cairoScope $ do
                setColor (mathematica97 i)
                sketch (Circle closest 3) >> fill
                sketch (Circle needle 3) >> fill
            cairoScope $ do
                setColor black
                sketch (Circle closest 3) >> stroke
                sketch (Circle needle 3) >> stroke
            cairoScope $ do
                setColor (mathematica97 i)
                sketch (Line needle closest) >> stroke
