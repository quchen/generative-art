-- | Port of Delaunator, a very fast Delaunay triangulation algorithm.
-- * Original Javascript source: https://github.com/mapbox/delaunator
-- * Rust port used as second reference: https://github.com/mourner/delaunator-rs
--
-- This module aims to be as dumb and close to the Rust source implementation as
-- possible. Users of this internal module are responsible for implementing a nice
-- API.
module Geometry.Algorithms.Delaunay.Delaunator where



import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.Function
import           Data.Ord
import           Data.STRef
import           Data.Vector                  (Vector, (!))
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as VM
import           Data.Vector.Mutable          (STVector)
import qualified Data.Vector.Mutable          as VM
import           Debug.Trace
import           GHC.Stack                    (HasCallStack)
import           Geometry.Core



-- | Near-duplicate points (where both @x@ and @y@ only differ within this value)
-- will not be included in the triangulation for robustness.
epsilon :: Double
epsilon = 2*ieee_float64_epsilon
  where
    -- Got this by printing f64::EPSILON in a Rust repl.it.
    -- Couldn’t find it in Haskell’s base.
    ieee_float64_epsilon = 2.2204460492503131e-16

distSquare :: Vec2 -> Vec2 -> Double
distSquare x y = normSquare (x -. y)

-- | In screen coordinates (y downward),
--
-- * Returns a negative value if @x@, @q@ and @r@ occur in counterclockwise order
--   (@r@ is to the left of the directed line @x@ --> @q@)
-- * Returns a positive value if they occur in clockwise order
--   (@r@ is to the right of the directed line @x@ --> @q@)
-- * Returns zero is they are collinear
orient :: Vec2 -> Vec2 -> Vec2 -> Double
orient x q r = negate (cross lineQ lineR)
  where
    lineQ = q -. x
    lineR = r -. x

-- | Offset of the circumcenter of the triangle (a,b,c) from a.
circumdelta :: Vec2 -> Vec2 -> Vec2 -> Vec2
circumdelta (Vec2 ax ay) (Vec2 bx by) (Vec2 cx cy) =
    let dx = bx - ax;
        dy = by - ay;
        ex = cx - ax;
        ey = cy - ay;

        bl = dx*dx + dy*dy;
        cl = ex*ex + ey*ey;
        d = 0.5 / (dx*ey - dy*ex);

        x = (ey*bl - dy*cl)*d;
        y = (dx*cl - ex*bl)*d;
    in Vec2 x y

-- | Square of the circumradius’ norm of the triangle (a,b,c).
circumradiusSquare
    :: Vec2 -- ^ a
    -> Vec2 -- ^ b
    -> Vec2 -- ^ c
    -> Double
circumradiusSquare a b c = normSquare (circumdelta a b c)

-- | Coordinate of the circumcenter of the triangle (a,b,c).
circumcenter
    :: Vec2 -- ^ a
    -> Vec2 -- ^ b
    -> Vec2 -- ^ c
    -> Vec2
circumcenter a b c = a +. circumdelta a b c

-- | Check whether a point is inside the circumcircle of a triangle.
inCircle
    :: Vec2 -- ^ Corner a
    -> Vec2 -- ^ Corner b
    -> Vec2 -- ^ Corner c
    -> Vec2 -- ^ Point to check
    -> Bool
inCircle (Vec2 ax ay) (Vec2 bx by) (Vec2 cx cy) (Vec2 px py) =
    let dx = ax - px;
        dy = ay - py;
        ex = bx - px;
        ey = by - py;
        fx = cx - px;
        fy = cy - py;

        ap' = dx*dx + dy*dy; -- ap shadowed by Control.Monad.ap
        bp  = ex*ex + ey*ey;
        cp  = fx*fx + fy*fy;

    in dx*(ey*cp - bp*fy) - dy*(ex*cp - bp*fx) + ap'*(ex*fy - ey*fx) < 0

nearlyEquals :: Vec2 -> Vec2 -> Bool
nearlyEquals a b =
    let Vec2 dx dy = a -. b
    in abs dx <= epsilon && abs dy <= epsilon

-- Type synonym so copy+paste is less ambiguous from the Rust code
type USize = Int

-- Represents the area outside of the triangulation. Halfedges on the convex hull
-- (which don't have an adjacent halfedge) will have this value.
tEMPTY :: USize
tEMPTY = min 1000000 maxBound

-- | Given one halfedge, go to the next one. This allows walking around a single
-- triangle.
nextHalfedge :: USize -> USize
nextHalfedge i = if mod i 3 == 2 then i-2 else i+1

-- | Inverse of 'nextHalfedge'.
prevHalfedge :: USize -> USize
prevHalfedge i = if mod i 3 == 0 then i+2 else i-1

data TriangulationST s = TriangulationST
    { _triangles :: STVector s USize
    -- ^ A vector of point indices where each triple represents a Delaunay triangle.
    -- All triangles are directed counter-clockwise.
    --
    -- The i-th triangle is points[3i] points[3i+1] points[3i+2].

    , _numTriangles :: STRef s USize
    -- ^ Number of triangles. The number of valid entries in '_triangles' is 3*'_numTriangles'.

    , _halfedges :: STVector s USize
    -- ^ A vector of adjacent halfedge indices that allows traversing the triangulation graph.
    --
    -- `i`-th half-edge in the array corresponds to vertex `triangles[i]`
    -- the half-edge is coming from. `halfedges[i]` is the index of a twin half-edge
    -- in an adjacent triangle (or `EMPTY` for outer half-edges on the convex hull).

    , _hull :: STVector s USize
    -- ^ A vector of indices that reference points on the convex hull of the triangulation,
    -- counter-clockwise.
    , _hullLen :: STRef s USize
    }

data Triangulation = Triangulation
    { __triangles :: Vector USize
    , __halfedges :: Vector USize
    , __hull :: Vector USize
    } deriving (Eq, Ord, Show)

freezeTriangulation :: HasCallStack => TriangulationST s -> ST s Triangulation
freezeTriangulation tgl = do
    triangles <- V.freeze (_triangles tgl)
    halfedges <- V.freeze (_halfedges tgl)

    hull <- V.freeze (_hull tgl)
    pure Triangulation
        { __triangles = triangles
        , __halfedges = halfedges
        , __hull = const V.empty hull -- TODO fix this once the hull doesn’t crash anymore
        }

freezeShrinkTriangulation :: HasCallStack => TriangulationST s -> ST s Triangulation
freezeShrinkTriangulation tgl = do
    numTriangles <- readSTRef (_numTriangles tgl)
    triangles <- V.freeze (VM.take numTriangles (_triangles tgl))
    halfedges <- V.freeze (VM.take numTriangles (_halfedges tgl))

    hullLen <- readSTRef (_hullLen tgl)
    hull <- V.freeze (VM.take hullLen (_hull tgl))
    pure Triangulation
        { __triangles = triangles
        , __halfedges = halfedges
        , __hull = const V.empty hull -- TODO fix this once the hull doesn’t crash anymore
        }

{-# DEPRECATED newVectorWithGoodErrorMessages "Use VM.unsafeNew instead once the code works" #-}
newVectorWithGoodErrorMessages :: HasCallStack => String -> Int -> ST s (STVector s USize)
newVectorWithGoodErrorMessages name n = case debugMode of
    Chatty -> VM.generate n (\i -> error ("Uninitialized element " ++ show i ++ " in vector " ++ name))
    NonsenseValue -> VM.generate n (\i -> 200000+i)
    DebuggedAndUnsafe -> VM.unsafeNew n
  where
    debugMode = NonsenseValue

data DebugMode = Chatty | NonsenseValue | DebuggedAndUnsafe

triangulation_new
    :: HasCallStack
    => USize -- ^ Number of points
    -> ST s (TriangulationST s)
triangulation_new n = do
    let maxTriangles = if n > 2 then 2*n-5 else 0
    triangles <- newVectorWithGoodErrorMessages "triangulation.triangles" (maxTriangles*3)
    numTriangles <- newSTRef 0
    halfedges <- newVectorWithGoodErrorMessages "triangulation.halfedges" (maxTriangles*3)
    hull <- newVectorWithGoodErrorMessages "triangulation.hull" n
    hullLenRef <- newSTRef 0
    pure TriangulationST
        { _triangles = triangles
        , _numTriangles = numTriangles
        , _halfedges = halfedges
        , _hull = hull
        , _hullLen = hullLenRef
        }

-- ^ Number of triangles in the triangulation.
--
--
-- The name can be misunderstood as the number of valid entries in the _triangles
-- array (which is 3*triangulation_len), but is being kept to stay coherent with
-- the Rust source.
triangulation_len :: TriangulationST s -> ST s USize
triangulation_len TriangulationST{_numTriangles = lenRef} = readSTRef lenRef

triangulation_is_empty :: TriangulationST s -> ST s Bool
triangulation_is_empty tri = fmap (== 0) (triangulation_len tri)

-- | Add a new triangle to the triangulation; report the old (!) size (why, Rust source?!).
triangulation_add_triangle
    :: HasCallStack
    => TriangulationST s
    -> USize -- ^ Corner i0
    -> USize -- ^ Corner i1
    -> USize -- ^ Corner i2
    -> USize -- ^ Halfedge a
    -> USize -- ^ Halfedge b
    -> USize -- ^ Halfedge c
    -> ST s USize
triangulation_add_triangle tgl i0 i1 i2 a b c = do
    t <- triangulation_len tgl

    VM.write (_triangles tgl)  t    i0
    VM.write (_triangles tgl) (t+1) i1
    VM.write (_triangles tgl) (t+2) i2
    modifySTRef' (_numTriangles tgl) (+1)

    VM.write (_halfedges tgl)  t    a
    VM.write (_halfedges tgl) (t+1) b
    VM.write (_halfedges tgl) (t+2) c

    when (a /= tEMPTY) (VM.write (_halfedges tgl) a  t   )
    when (b /= tEMPTY) (VM.write (_halfedges tgl) b (t+1))
    when (c /= tEMPTY) (VM.write (_halfedges tgl) c (t+2))

    pure t

triangulation_legalize
    :: HasCallStack
    => TriangulationST s
    -> USize
    -> Vector Vec2
    -> Hull s
    -> ST s USize
triangulation_legalize tgl !a points hull = do
    b <- VM.read (_halfedges tgl) a

    -- If the pair of triangles doesn't satisfy the Delaunay condition (p1 is
    -- inside the circumcircle of [p0, pl, pr]), flip them, then do the same
    -- check/flip recursively for the new pair of triangles
    --
    --           pl                    pl
    --          /||\                  /  \
    --       al/ || \bl            al/    \a
    --        /  ||  \              /      \
    --       /  a||b  \    flip    /___ar___\
    --     p0\   ||   /p1   =>   p0\---bl---/p1
    --        \  ||  /              \      /
    --       ar\ || /br             b\    /br
    --          \||/                  \  /
    --           pr                    pr
    --
    let ar = prevHalfedge a

    case b == tEMPTY of
        True -> pure ar
        False -> do {
    ; do
        let al = nextHalfedge a
            bl = prevHalfedge b

        p0 <- VM.read (_triangles tgl) ar
        pr <- VM.read (_triangles tgl) a
        pl <- VM.read (_triangles tgl) al
        p1 <- VM.read (_triangles tgl) bl

        let illegal = inCircle (points!p0) (points!pr) (points!pl) (points!p1)
        case illegal of
            False -> pure ar
            True -> do {
    ; do
        VM.write (_triangles tgl) a p1
        VM.write (_triangles tgl) b p0

        hbl <- VM.read (_halfedges tgl) bl
        har <- VM.read (_halfedges tgl) ar

        -- // edge swapped on the other side of the hull (rare); fix the halfedge reference
        when (hbl == tEMPTY) $ do
            eRef <- newSTRef =<< readSTRef (_start hull)
            fix $ \loop -> do
                e <- readSTRef eRef
                hull_tri_e <- VM.read (_tri hull) e
                if hull_tri_e == bl
                    then do
                        VM.write (_tri hull) e a
                        pure () -- break
                    else do
                        hull_prev_e <- VM.read (_prev hull) e
                        let e' = hull_prev_e
                        writeSTRef eRef e'
                        hull_start <- readSTRef (_start hull)
                        if e == hull_start
                            then pure () -- break
                            else loop

        VM.write (_halfedges tgl) a hbl
        VM.write (_halfedges tgl) b har
        VM.write (_halfedges tgl) ar bl

        when (hbl /= tEMPTY) (VM.write (_halfedges tgl) hbl a)
        when (har /= tEMPTY) (VM.write (_halfedges tgl) har b)
        when (bl /= tEMPTY) (VM.write (_halfedges tgl) bl ar)

        let br = nextHalfedge b
        _ <- triangulation_legalize tgl a points hull
        triangulation_legalize tgl br points hull
    }}

data Hull s = Hull
    { _prev :: STVector s USize -- ^ edge to prev edge
    , _next :: STVector s USize -- ^ edge to next edge
    , _tri :: STVector s USize  -- ^ edge to adjacent halfedge
    , _hash :: STVector s USize -- ^ angular edge hash
    , _hashLen :: Int
    , _start :: STRef s USize
    , _center :: Vec2
    }

hull_new
    :: HasCallStack
    => USize -- ^ Number of points.
             -- (Redundant since we’ve also got the input points vector,
             -- but the Rust source does it this way.)
    -> Vec2  -- ^ Circumcenter of the initial triangle
    -> USize -- ^ First corner of the initial triangle
    -> USize -- ^ Second corner of the initial triangle
    -> USize -- ^ Third corner of the initial triangle
    -> Vector Vec2 -- ^ Input points
    -> ST s (Hull s)
hull_new n center i0 i1 i2 points = do
    let hash_len :: USize
        hash_len = floor (sqrt (fromIntegral n))

    prev <- VM.replicate n 0
    next <- VM.replicate n 0
    tri <- VM.replicate n 0
    hash <- VM.replicate hash_len tEMPTY
    i0Ref <- newSTRef i0

    let hull = Hull
            { _prev = prev
            , _next = next
            , _tri = tri
            , _hash = hash
            , _hashLen = hash_len
            , _start = i0Ref
            , _center = center
            }

    VM.write next i0 i1
    VM.write prev i2 i1
    VM.write next i1 i2
    VM.write prev i0 i2
    VM.write next i2 i0
    VM.write prev i1 i0

    VM.write tri i0 0
    VM.write tri i1 1
    VM.write tri i2 2

    hull_hash_edge hull (points ! i0) i0
    hull_hash_edge hull (points ! i1) i1
    hull_hash_edge hull (points ! i2) i2

    pure hull

hull_hash_key
    :: HasCallStack
    => Hull s
    -> Vec2
    -> ST s USize
hull_hash_key hull p = do
    let a = pseudoAngle (p -. _center hull)
        len = _hashLen hull
    pure ((floor((fromIntegral len :: Double) * a) :: USize) `mod` len)

hull_hash_edge
    :: HasCallStack
    => Hull s
    -> Vec2
    -> USize
    -> ST s ()
hull_hash_edge hull p i = do
    key <- hull_hash_key hull p
    VM.write (_hash hull) key i

hull_find_visible_edge
    :: HasCallStack
    => Hull s
    -> Vec2
    -> Vector Vec2
    -> ST s (USize, Bool)
hull_find_visible_edge hull p points = do
    startRef <- newSTRef 0
    key <- hull_hash_key hull p
    let len = _hashLen hull

    do  let loop j | j >= len = pure () -- end of loop
            loop j = do
                start <- VM.read (_hash hull) ((key+j) `mod` len)
                writeSTRef startRef start
                next <- VM.read (_next hull) start
                if start /= tEMPTY && next /= tEMPTY
                    then pure () -- break
                    else loop (j+1)
        loop 0

    start <- do
        oldStart <- readSTRef startRef
        VM.read (_prev hull) oldStart
    eRef <- newSTRef start

    fix $ \loop -> do
        e <- readSTRef eRef
        next_e <- VM.read (_next hull) e
        if orient p (points ! e) (points ! next_e) <= 0
            then do
                writeSTRef eRef next_e
                if next_e == start
                    then pure (tEMPTY, False)
                    else loop
            else pure (e, e == start)

calc_bbox_center :: Vector Vec2 -> Vec2
calc_bbox_center = boundingBoxCenter

-- | Find the closest point to a reference in the vector that is unequal to the
-- point itself.
find_closest_point :: HasCallStack => Vector Vec2 -> Vec2 -> Maybe USize
find_closest_point points p0 =
    let (minDist, minIx) = search (1/0) 0 0
    in if isInfinite minDist
        then Nothing
        else Just minIx
  where
    -- Explicit recursion makes excluding zero-distance points easier than going
    -- the map/filter/minInxedBy route.
    search !minDist !minIx !searchIx
        | searchIx >= V.length points = (minDist, minIx)
    search !minDist !minIx !searchIx =
        let p = points!searchIx
            d = distSquare p p0
        in if d > 0 && d < minDist
            then search d searchIx (searchIx+1)
            else search minDist minIx (searchIx+1)

find_seed_triangle :: HasCallStack => Vector Vec2 -> Maybe (USize, USize, USize)
find_seed_triangle points = do
    -- // pick a seed point close to the center
    let bboxCenter = calc_bbox_center points
    i0 <- find_closest_point points bboxCenter
    let p0 = points!i0

    -- // find the point closest to the seed
    i1 <- find_closest_point points p0
    let p1 = points!i1

    -- // find the third point which forms the smallest circumcircle with the first two
    let (minRadius, i2) = runST $ do
            minRadiusRef <- newSTRef Nothing
            i2Ref <- newSTRef 0
            for_ (V.indexed points) $ \(i, p) -> do
                case i == i0 || i == i1 of
                    True -> pure () -- continue
                    False -> do
                        let r = circumradiusSquare p0 p1 p
                        m'minRadius <- readSTRef minRadiusRef
                        case m'minRadius of
                            Nothing -> do
                                writeSTRef i2Ref i
                                writeSTRef minRadiusRef (Just r)
                            Just minRadius' | r < minRadius' -> do
                                writeSTRef i2Ref i
                                writeSTRef minRadiusRef (Just r)
                            _else -> pure ()

            minRadius' <- readSTRef minRadiusRef
            i2' <- readSTRef i2Ref
            pure (minRadius', i2')

    case minRadius of
        Nothing -> Nothing
        Just _ | orient p1 p1 (points!i2) > 0 -> Just (i0, i2, i1)
        _else -> Just (i0, i1, i2)

sortf :: STVector s (uSize, Double) -> ST s ()
sortf = VM.sortBy (comparing (\(_, d) -> d))

-- /// Order collinear points by dx (or dy if all x are identical) and return the list as a hull
-- fn handle_collinear_points(points: &[Point]) -> Triangulation {
--     let Point { x, y } = points.first().cloned().unwrap_or_default();
--
--     let mut dist: Vec<_> = points
--         .iter()
--         .enumerate()
--         .map(|(i, p)| {
--             let mut d = p.x - x;
--             if d == 0.0 {
--                 d = p.y - y;
--             }
--             (i, d)
--         })
--         .collect();
--     sortf(&mut dist);
--
--     let mut triangulation = Triangulation::new(0);
--     let mut d0 = f64::NEG_INFINITY;
--     for (i, distance) in dist {
--         if distance > d0 {
--             triangulation.hull.push(i);
--             d0 = distance;
--         }
--     }
--
--     triangulation
-- }

-- | Triangulate a set of 2D points. Returns the triangulation for the input
-- points. For the degenerated case when all points are collinear, returns an empty
-- triangulation where all points are in the hull.
triangulate :: HasCallStack => Vector Vec2 -> ST s (TriangulationST s)
triangulate points = do
    case find_seed_triangle points of
        Nothing -> error "Can’t find a seed triangle, and handle_collinear_points is not implemented"
        Just seed_triangle -> triangulate_for_real seed_triangle
  where
    triangulate_for_real :: (Int, Int, Int) -> ST s (TriangulationST s)
    triangulate_for_real seed_triangle = do
        let numPoints = V.length points
            (i0, i1, i2) = seed_triangle
            center = circumcenter (points!i0) (points!i1) (points!i2)

        tgl <- triangulation_new numPoints
        _ <- triangulation_add_triangle tgl i0 i1 i2 tEMPTY tEMPTY tEMPTY

        -- // sort the points by distance from the seed triangle circumcenter
        let dists = V.modify sortf (V.imap (\i p -> (i, distSquare center p)) points)

        hull <- hull_new numPoints center i0 i1 i2 points

        -- Those indices, argh!
        -- k: the loop runs over the k-th closest input point to the seed triangle center.
        -- i: index of the current subject point, as in »we are looking at point[i]«
        V.iforM_ dists $ \k (i, _) -> do
            let p = points!i
                isNearDuplicate =
                    let (previousI, _previousDist) = dists!(k-1)
                        previousP = points!previousI
                    in k > 0 && nearlyEquals p previousP -- k>0 so we don’t underrun in the k-1 lookup
                isInSeedTriangle = i == i0 || i == i1 || i == i2

            -- // skip near-duplicates
            -- // skip seed triangle points
            if isNearDuplicate || isInSeedTriangle
                then pure () -- continue
                else do {

            -- // find a visible edge on the convex hull using edge hash
            ; (e0, walk_back) <- hull_find_visible_edge hull p points
            ; traceShowM ("### Triangulate", e0, e0 == tEMPTY)

            ; if e0 == tEMPTY
                then pure () -- // likely a near-duplicate point; skip it
                else do {

            ; eRef <- newSTRef e0
            -- // add the first triangle from the point
            ; do
                e <- readSTRef eRef
                t <- do
                    hull_next_e <- VM.read (_next hull) e
                    hull_tri_e <- VM.read (_tri hull) e
                    triangulation_add_triangle tgl e i hull_next_e tEMPTY tEMPTY hull_tri_e

                -- // recursively flip triangles from the point until they satisfy the Delaunay condition
                VM.write (_tri hull) i =<< triangulation_legalize tgl (t+2) points hull
                VM.write (_tri hull) e t -- // keep track of boundary triangles on the hull

            -- // walk forward through the hull, adding more triangles and flipping recursively
            ; nRef <- do
                e <- readSTRef eRef
                hull_next_e <- VM.read (_next hull) e
                newSTRef hull_next_e
            ; do
                fix $ \loop -> do
                    n <- readSTRef nRef
                    q <- VM.read (_next hull) n
                    if orient p (points!n) (points!q) <= 0
                        then pure () -- break
                        else do
                            hull_tri_i <- VM.read (_tri hull) i
                            hull_tri_n <- VM.read (_tri hull) n
                            t <- triangulation_add_triangle tgl n i q hull_tri_i tEMPTY hull_tri_n
                            VM.write (_tri hull) i =<< triangulation_legalize tgl (t+2) points hull
                            VM.write (_next hull) n tEMPTY -- // mark as removed
                            writeSTRef nRef q
                            loop

            -- // walk backward from the other side, adding more triangles and flipping
            ; when walk_back $ do
                fix $ \loop -> do
                    e <- readSTRef eRef
                    q <- VM.read (_prev hull) e
                    if orient p (points!q) (points!e) <= 0
                        then pure () -- break
                        else do
                            hull_tri_e <- VM.read (_tri hull) e
                            hull_tri_q <- VM.read (_tri hull) q
                            t <- triangulation_add_triangle tgl q i e tEMPTY hull_tri_e hull_tri_q
                            _ <- triangulation_legalize tgl (t+2) points hull
                            VM.write (_tri hull) q t
                            VM.write (_next hull) e tEMPTY -- // mark as removed
                            writeSTRef eRef q
                            loop


            ; do
                -- // update the hull indices
                e <- readSTRef eRef
                n <- readSTRef nRef
                VM.write (_prev hull) i e
                VM.write (_next hull) i n
                VM.write (_prev hull) n i
                VM.write (_next hull) e i
                writeSTRef (_start hull) e

                -- // save the two new edges in the hash table
                hull_hash_edge hull p i
                hull_hash_edge hull (points!e) e

            -- // expose hull as a vector of point indices
            ; do
                eeRef <- newSTRef =<< readSTRef (_start hull)
                fix $ \loop -> do
                    hullPushIndex <- readSTRef (_hullLen tgl)
                    e <- readSTRef eeRef
                    VM.write (_hull tgl) hullPushIndex e
                    modifySTRef' (_hullLen tgl) (+1)
                    hull_next_e <- VM.read (_next hull) e
                    writeSTRef eeRef hull_next_e
                    hull_start <- readSTRef (_start hull)
                    unless (e == hull_start) loop

            }}

        pure tgl
