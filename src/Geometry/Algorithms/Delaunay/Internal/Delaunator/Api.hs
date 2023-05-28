-- | Nice API for Delaunator’s technical output.
module Geometry.Algorithms.Delaunay.Internal.Delaunator.Api where



import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Map               as M
import           Data.Vector            (Vector, (!))
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as VM
import           Geometry.Core
import           Numerics.Interpolation
import           Util

import qualified Geometry.Algorithms.Delaunay.Internal.Delaunator.Raw as D

import Draw
import Graphics.Rendering.Cairo as C




-- | Abstract data type supporting many efficient Delaunay and Voronoi properties.
data DelaunayTriangulation = Triangulation
    { _triangles             :: Vector Polygon
    , _edges                 :: Vector Line
    , _voronoiCorners        :: Vector Vec2
    , _voronoiEdges          :: Vector (Either Line Ray)
    , _voronoiCells          :: Vector VoronoiPolygon
    , _convexHull            :: Vector Vec2
    , _findClosestInputPoint :: Vec2 -> Int -> Int
    }

-- | Create a 'DelaunayTriangulation' from a set of points.
delaunayTriangulation :: Sequential vector => vector Vec2 -> DelaunayTriangulation
delaunayTriangulation points' =
    let points = toVector points'
        raw = D.triangulate points
        (triPolygons, triCircumcenters) = triangles points raw
        extRays = exteriorRays points raw
        inedges = bulidInedgesLookup raw
        hullIndex = createHullIndex points raw
        findTriangle needle i0 = findClosestInputPointIndex points inedges hullIndex raw needle i0
    in Triangulation
        { _triangles = triPolygons
        , _edges = edges points raw
        , _findClosestInputPoint = findTriangle
        , _voronoiCorners = triCircumcenters
        , _voronoiEdges = voronoiEdges triCircumcenters raw extRays
        , _voronoiCells = voronoiCells points triCircumcenters inedges raw
        , _convexHull = convexHullViaDelaunay points raw
        }

instance NFData DelaunayTriangulation where
    rnf Triangulation
        { _triangles = x1
        , _edges = x2
        , _findClosestInputPoint = _
        , _voronoiCorners = x3
        , _voronoiEdges = x4
        , _voronoiCells = x5
        , _convexHull = x6
        } = rnf (x1, x2, x3, x4, x5, x6)

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

edges :: Vector Vec2 -> D.TriangulationRaw -> Vector Line
edges points triangulation = do
    let triangleIxs = D._triangles triangulation
        halfedges = D._halfedges triangulation
        numHalfedges = V.length halfedges
    e <- V.enumFromN 0 numHalfedges

    -- We arbitrarily select the larger of the two edges here. Note that this also
    -- covers the pair-less case, in which the opposite halfedge has index -1.
    guard (e > halfedges!e)
    let p = points!(triangleIxs!e)
        q = points!(triangleIxs!D.nextHalfedge e)
    pure (Line p q)

convexHullViaDelaunay :: Vector Vec2 -> D.TriangulationRaw -> Vector Vec2
convexHullViaDelaunay points triangulation = V.backpermute points (D._convexHull triangulation)

-- ^ Given a single edge, what’s the index of the start of the triangle?
triangleOfEdge :: Int -> Int
triangleOfEdge e = div e 3

voronoiEdges :: Vector Vec2 -> D.TriangulationRaw -> Vector ExtRays -> Vector (Either Line Ray)
voronoiEdges circumcenters triangulation extRays = do
    let halfedges = D._halfedges triangulation
        numHalfedges = V.length halfedges

    e <- V.enumFromN 0 numHalfedges
    let e' = halfedges!e
        pStart = circumcenters ! triangleOfEdge e
    if
        | e' == D.tEMPTY ->
            let ExtRays _inDir outDir = extRays ! (D._triangles triangulation ! e)
                -- I don’t know why it’s outDir and not inDir, but I’m quite happy
                -- it’s consistent in my tests. I would have expected more random
                -- behavior. Lucky me!
            in pure (Right (Ray pStart outDir))
        | e < e' ->
            let pEnd = circumcenters ! triangleOfEdge e'
            in pure (Left (Line pStart pEnd))
        | otherwise -> mempty

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
        -- infinity at the beginning\/end in the direction of the first\/last
        -- argument. For example, the bottom\/right quadrant (in screen coordinates)
        -- would be @'VoronoiInfinite' ('Vec2' 0 1) ['Vec2' 0 0] ('Vec2' 1 0)@.
    deriving (Eq, Ord, Show)

instance Sketch VoronoiPolygon where
    sketch (VoronoiFinite polygon) = sketch polygon
    sketch (VoronoiInfinite _ [] _) = pure ()
    sketch (VoronoiInfinite dirIn points dirOut) = do
        let sketchRay start dir = cairoScope $ setDash [3,3] 0 >> sketch (resizeLine (const 30) (Line start (start +. dir)))
        sketchRay (head points) dirIn
        sketch (Polyline points)
        sketchRay (head points) dirOut

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

voronoiCells :: Vector Vec2 -> Vector Vec2 -> M.Map Int Int -> D.TriangulationRaw -> Vector VoronoiPolygon
voronoiCells points circumcenters inedges delaunay =
    let extRays = exteriorRays points delaunay
    in flip V.imap points $ \pIx _pCoord ->
        let incoming = inedges M.! pIx
        in voronoiPolygon circumcenters delaunay extRays pIx incoming

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

-- | A ray is a line that extends to infinity on one side.
data Ray = Ray !Vec2 !Vec2 -- ^ Starting point and direction (!)
    deriving (Eq, Ord, Show)

instance NFData Ray where
    rnf _ = () -- Already strict

instance Sketch Ray where
    sketch (Ray start dir) = sketch (resizeLine (const 100000) (Line start (start +. dir)))

-- | Reverse lookup table for the hull. @'D._convexHull'!i@ yields the i-th point’s
-- ID on the hull, this index gives us the number i, given a point ID.
createHullIndex :: Vector Vec2 -> D.TriangulationRaw -> Vector Int
createHullIndex points raw = V.create $ do
    let hull = D._convexHull raw
    hullIndex <- VM.replicate (V.length points) (-1)
    V.iforM_ hull $ \i hull_i -> VM.write hullIndex hull_i i
    pure hullIndex

-- | Find the input point closest to the needle. Search starts at specified point i.
findClosestInputPointIndex
    :: Vector Vec2   -- ^ Input points
    -> M.Map Int Int -- ^ Incoming edges table
    -> Vector Int    -- ^ Hull index, see 'createHullIndex'
    -> D.TriangulationRaw
    -> Vec2          -- ^ Needle: which input point is closest to this?
    -> Int           -- ^ Start the search at this index. 0 searches from the beginning.
    -> Int           -- ^ Index of the closest point
findClosestInputPointIndex points inedges hullIndex tri needle i0 = loopFind i0
  where
    loopFind i =
        let c = step i
        in if c >= 0 && c /= i && c /= i0
            then loopFind c
            else c

    -- The idea of one step is to look at outgoing edges of a point, and following
    -- the one that leads us closer to the needle.
    step j =
        let c = j
            dc = normSquare (needle -. points!j)
            e0 = inedges M.! j
            e = e0
        in loopStep j c dc e0 e

    loopStep
        :: Int
        -> Int    -- c:  Start of search (candidate)
        -> Double -- dc: Distance² from candidate to needle
        -> Int    -- e0: inedge the search has started
        -> Int    -- e:  inedge we’re currently searching
        -> Int    -- Better candidate after the step
    loopStep j c dc e0 e =
        let t = D._triangles tri ! e
            dt = normSquare (needle -. points!t)
            (dc', c') | dt < dc   = (dt, t)
                      | otherwise = (dc, c)
            e' = D._halfedges tri ! D.nextHalfedge e
        in if e' == D.tEMPTY
            then -- The next edge has no partner: we’re on the hull
                let e'' = D._convexHull tri ! (((hullIndex!j) + 1) `mod` V.length (D._convexHull tri))
                in if e'' /= t && normSquare (needle -. points!e'') < dc'
                    then e''
                    else c'
            else -- We’re not on the hull
                if e' /= e0
                    then loopStep j c' dc' e0 e'
                    else c'

-- | Voronoi stippling. Unfortunately not fast enough for stippling images
-- pixel-wise, hence not exposed from the user-facing module.
stipple
    :: Double -- ^ \(\omega\) convergence speed parameter, see 'lloydRelaxation'.
    -> Int -- ^ Width of the input data. x values will be picked in the integer range \([0\ldots w)\).
    -> Int -- ^ Height of the input data. x values will be picked in the integer range \([0\ldots h)\).
    -> (Int -> Int -> Double) -- ^ How much weight does the input have at \(f(x,y)\)?
    -> Vector Vec2 -- ^ Input points, chosen as last parameter for 'iterate' convenience.
    -> Int -- ^ Number of generations
    -> Vector Vec2
stipple omega width height f points n = iterate (stippleStep omega width height f) points !! n

-- | One step towards Voronoi stippling. See 'stipple' for argument docs.
stippleStep
    :: Double
    -> Int
    -> Int
    -> (Int -> Int -> Double)
    -> Vector Vec2
    -> Vector Vec2
stippleStep omega width height f points = runST $ do
    let numPoints = V.length points
        tri = delaunayTriangulation points

    centroidsMut <- VM.replicate numPoints zero
    weightsMut <- VM.replicate numPoints 0
    let loopY _ y | y >= height = pure ()
        loopY i y = loopX i 0 y

        loopX i x y | x >= width = loopY i (y+1)
        loopX i x y = do
            let w = f x y
                pixelTopLeft = Vec2 (fromIntegral x) (fromIntegral y)
                pixelCenter = pixelTopLeft +. Vec2 0.5 0.5
                i' = _findClosestInputPoint tri pixelTopLeft i
            VM.modify weightsMut (+w) i'
            VM.modify centroidsMut (+. (w *. pixelCenter)) i'
            loopX i' (x+1) y
    loopY 0 0

    centroids <- V.unsafeFreeze centroidsMut
    weights <- V.unsafeFreeze weightsMut

    pure $ V.zipWith3
        (\c w p ->
            let p' | w > 0 = c /. w
                   | otherwise = p
            in lerp (0,1) (p,p') omega
        )
        centroids
        weights
        points
