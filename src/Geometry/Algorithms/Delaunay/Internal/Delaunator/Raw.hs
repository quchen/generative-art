-- | Port of Delaunator, a very fast Delaunay triangulation algorithm.
-- * Original Javascript source: https://github.com/mapbox/delaunator
-- * Rust port used as second reference: https://github.com/mourner/delaunator-rs
--
-- This module aimed to be as dumb and close to the Rust source implementation as
-- possible. Refactor as long as the tests pass.
--
-- Users of this internal module are responsible for implementing a nice API.
module Geometry.Algorithms.Delaunay.Internal.Delaunator.Raw where



import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.Function
import           Data.Ord
import           Data.STRef
import           Data.Vector                (Vector, (!))
import qualified Data.Vector                as V
import qualified Data.Vector.Algorithms.Tim as VM
import           Data.Vector.Mutable        (STVector)
import qualified Data.Vector.Mutable        as VM
import           GHC.Stack                  (HasCallStack)
import           Geometry.Core



-- ^ Squared distance between two points
distSquare :: Vec2 -> Vec2 -> Double
distSquare x y = normSquare (x -. y)

-- | Orientation in screen coordinates (y downward).
data Orientation = Clockwise | Counterclockwise | Degenerate
    deriving (Eq, Ord, Show)

-- | Orientation of a triangle in screen coordinates (y downward).
--
-- >>> (a,b,c) = (Vec2 0 0, Vec2 100 0, Vec2 0 100)
-- >>> orientation a b c
-- Clockwise
-- >>> orientation b a c
-- Counterclockwise
orientation :: Vec2 -> Vec2 -> Vec2 -> Orientation
orientation x q r =
    let lineQ = q -. x
        lineR = r -. x
    in case compare (cross lineQ lineR) 0 of
        GT -> Clockwise
        EQ -> Degenerate
        LT -> Counterclockwise

-- | Offset of the circumcenter of the triangle (a,b,c) from a.
--
-- >>> (a,b,c) = (Vec2 0 0, Vec2 100 0, Vec2 0 100)
-- >>> circumdelta a b c
-- Vec2 50.0 50.0
circumdelta :: Vec2 -> Vec2 -> Vec2 -> Vec2
circumdelta a b c =
    let Vec2 dx dy = b -. a
        Vec2 ex ey = c -. a

        bl = dx*dx + dy*dy;
        cl = ex*ex + ey*ey;
        d = 0.5 / (dx*ey - dy*ex);

        x = (ey*bl - dy*cl)*d;
        y = (dx*cl - ex*bl)*d;
    in Vec2 x y

-- | Square of the circumradius’ norm of the triangle (a,b,c).
--
-- >>> (a,b,c) = (Vec2 0 0, Vec2 100 0, Vec2 0 100)
-- >>> circumradiusSquare a b c
-- 5000.0
circumradiusSquare :: Vec2 -> Vec2 -> Vec2 -> Double
circumradiusSquare a b c = normSquare (circumdelta a b c)

-- | Coordinate of the circumcenter of the triangle (a,b,c).
--
-- >>> (a,b,c) = (Vec2 0 0, Vec2 100 0, Vec2 0 100)
-- >>> circumcenter a b c
-- Vec2 50.0 50.0
circumcenter :: Vec2 -> Vec2 -> Vec2 -> Vec2
circumcenter a b c = a +. circumdelta a b c

-- | Check whether a point is inside the circumcircle of a triangle. The triangle
-- must be oriented in counter-clockwise orientation in screen coordinates.
--
-- >>> abc@(a,b,c) = (Vec2 0 0, Vec2 0 100, Vec2 100 0)
--
-- >>> orientation a b c
-- Counterclockwise
--
-- >>> inCircle abc (Vec2 25 25)
-- True
--
-- >>> inCircle abc (Vec2 150 150)
-- False
inCircle
    :: (Vec2, Vec2, Vec2) -- ^ Triangle’s corners
    -> Vec2 -- ^ Point
    -> Bool
inCircle (a, b, c) p =
    let d@(Vec2 dx dy) = a -. p
        e@(Vec2 ex ey) = b -. p
        f@(Vec2 fx fy) = c -. p

        ap' = normSquare d -- ap shadowed by Control.Monad.ap
        bp  = normSquare e
        cp  = normSquare f

        val = dx*(ey*cp - bp*fy) - dy*(ex*cp - bp*fx) + ap'*(ex*fy - ey*fx)

    in val < 0

-- | Are both axes at most 'epsilon' apart?
nearlyEquals :: Vec2 -> Vec2 -> Bool
nearlyEquals a b =
    let Vec2 dx dy = a -. b
    in abs dx <= epsilon && abs dy <= epsilon
  where
    epsilon :: Double
    epsilon = 2*ieee_float64_epsilon

    ieee_float64_epsilon :: Double
    -- Got this by printing f64::EPSILON in a Rust repl.it.
    -- Couldn’t find it in Haskell’s Base.
    ieee_float64_epsilon = 2.2204460492503131e-16

-- | Represents the area outside of the triangulation. Halfedges on the convex hull
-- (which don't have an adjacent halfedge) will have this value.
tEMPTY :: Int
tEMPTY = -1

-- | Given one halfedge, go to the next one. This allows walking around a single
-- triangle.
nextHalfedge :: Int -> Int
nextHalfedge i = if mod i 3 == 2 then i-2 else i+1
{-# INLINE nextHalfedge #-}

-- | Inverse of 'nextHalfedge'.
prevHalfedge :: Int -> Int
prevHalfedge i = if mod i 3 == 0 then i+2 else i-1
{-# INLINE prevHalfedge #-}

data TriangulationST s = TriangulationST
    { __triangles :: STVector s Int
    -- ^ A vector of point indices where each triple represents a Delaunay
    -- triangle. All triangles are directed counter-clockwise (in screen
    -- coordinates, i.e. y pointing downwards).
    --
    --   The i-th triangle is points[3i], points[3i+1], points[3i+2].

    , __trianglesLen :: STRef s Int
    -- ^ Number of valid entries in '__triangles'. n/3 is the number of triangles
    --   in the triangulation.

    , __halfedges :: STVector s Int
    -- ^ A vector of adjacent halfedge indices that allows traversing the triangulation graph.
    --
    -- `i`-th half-edge in the array corresponds to vertex `triangles[i]`
    -- the half-edge is coming from. `halfedges[i]` is the index of a twin half-edge
    -- in an adjacent triangle (or `tEMPTY` for outer halfedges on the convex hull).
    }

{-# DEPRECATED newVectorWithGoodErrorMessages "Use VM.unsafeNew instead once the code works" #-}
newVectorWithGoodErrorMessages :: HasCallStack => String -> Int -> ST s (STVector s Int)
newVectorWithGoodErrorMessages name n = case debugMode of
    Chatty -> VM.generate n (\i -> error ("Uninitialized element " ++ show i ++ " in vector " ++ name))
    NonsenseValue -> VM.generate n (\i -> 200000+i)
    DebuggedAndUnsafe -> VM.unsafeNew n
  where
    debugMode = Chatty

data DebugMode = Chatty | NonsenseValue | DebuggedAndUnsafe

-- | Initialize a new mutable triangulation struct.
triangulation_new
    :: HasCallStack
    => Int -- ^ Number of points
    -> ST s (TriangulationST s)
triangulation_new n = do
    let maxTriangles = if n > 2 then 2*n-5 else 0
    triangles <- newVectorWithGoodErrorMessages "triangulation.triangles" (maxTriangles*3)
    trianglesLen <- newSTRef 0
    halfedges <- newVectorWithGoodErrorMessages "triangulation.halfedges" (maxTriangles*3)
    pure TriangulationST
        { __triangles = triangles
        , __trianglesLen = trianglesLen
        , __halfedges = halfedges
        }

-- | Add a new triangle to the triangulation; report the old (!) size (why, Rust source?!).
triangulation_add_triangle
    :: HasCallStack
    => TriangulationST s
    -> Int -- ^ Corner i0
    -> Int -- ^ Corner i1
    -> Int -- ^ Corner i2
    -> Int -- ^ Halfedge a
    -> Int -- ^ Halfedge b
    -> Int -- ^ Halfedge c
    -> ST s Int
triangulation_add_triangle tgl i0 i1 i2 a b c = do
    t <- readSTRef (__trianglesLen tgl)

    VM.write (__triangles tgl)  t    i0
    VM.write (__triangles tgl) (t+1) i1
    VM.write (__triangles tgl) (t+2) i2
    modifySTRef' (__trianglesLen tgl) (+3)

    VM.write (__halfedges tgl)  t    a
    VM.write (__halfedges tgl) (t+1) b
    VM.write (__halfedges tgl) (t+2) c

    when (a /= tEMPTY) (VM.write (__halfedges tgl) a  t   )
    when (b /= tEMPTY) (VM.write (__halfedges tgl) b (t+1))
    when (c /= tEMPTY) (VM.write (__halfedges tgl) c (t+2))

    pure t

triangulation_legalize
    :: HasCallStack
    => TriangulationST s -- ^ Triangulation that needs legalization
    -> Int             -- ^ ID of the halfedge to potentially flip
    -> Vector Vec2       -- ^ Delaunay input points
    -> HullST s
    -> ST s Int -- ^ Halfedge adjacent to a. I don’t fully understand this value yet.
triangulation_legalize tgl !a points hull = do
    b <- VM.read (__halfedges tgl) a
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

    -- b is a’s opposite halfedge. If it’s not there, it can’t be wrong.
    case b == tEMPTY of
        True -> pure ar
        False -> do {
    ; do
        let al = nextHalfedge a
            bl = prevHalfedge b

        p0 <- VM.read (__triangles tgl) ar
        pr <- VM.read (__triangles tgl) a
        pl <- VM.read (__triangles tgl) al
        p1 <- VM.read (__triangles tgl) bl

        -- Delaunay condition: No point may be inside the circumcircle of another triangle.
        let illegal = inCircle (points!p0, points!pr, points!pl) (points!p1)
        case illegal of
            False -> pure ar
            True -> do {
    ; do
        VM.write (__triangles tgl) a p1
        VM.write (__triangles tgl) b p0

        hbl <- VM.read (__halfedges tgl) bl
        har <- VM.read (__halfedges tgl) ar

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
                        if e' /= hull_start
                            then loop
                            else pure () -- break

        VM.write (__halfedges tgl) a hbl
        VM.write (__halfedges tgl) b har
        VM.write (__halfedges tgl) ar bl

        when (hbl /= tEMPTY) (VM.write (__halfedges tgl) hbl a)
        when (har /= tEMPTY) (VM.write (__halfedges tgl) har b)
        when (bl /= tEMPTY) (VM.write (__halfedges tgl) bl ar)

        let br = nextHalfedge b
        _ <- triangulation_legalize tgl a points hull
        triangulation_legalize tgl br points hull
    }}

data HullST s = Hull
    { _prev :: STVector s Int -- ^ Edge to previous edge
    , _next :: STVector s Int -- ^ Edge to next edge
    , _tri :: STVector s Int  -- ^ Edge to adjacent halfedge
    , _hash :: STVector s Int -- ^ angular edge hash
    , _hashLen :: Int
    , _start :: STRef s Int
    , _center :: Vec2
    }

hull_new
    :: HasCallStack
    => Int       -- ^ Number of points.
                 --   (Redundant since we’ve also got the input points vector,
                 --   but the Rust source does it this way.)
    -> Vec2      -- ^ Circumcenter of the initial triangle
    -> Int       -- ^ First corner of the initial triangle
    -> Int       -- ^ Second corner of the initial triangle
    -> Int       -- ^ Third corner of the initial triangle
    -> Vector Vec2 -- ^ Input points
    -> ST s (HullST s)
hull_new n center i0 i1 i2 points = do
    let hash_len :: Int
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

    -- Forward direction: i0 -> i1 -> i2 -> i0
    VM.write next i0 i1
    VM.write next i1 i2
    VM.write next i2 i0

    -- Backwards direction: i0 -> i2 -> i1 -> i0
    VM.write prev i0 i2
    VM.write prev i2 i1
    VM.write prev i1 i0

    VM.write tri i0 0
    VM.write tri i1 1
    VM.write tri i2 2

    hull_hash_edge hull (points!i0) i0
    hull_hash_edge hull (points!i1) i1
    hull_hash_edge hull (points!i2) i2

    pure hull

hull_hash_key
    :: HasCallStack
    => HullST s
    -> Vec2
    -> ST s Int
hull_hash_key hull p = do
    let a = pseudoAngle (p -. _center hull)
        len = _hashLen hull
    pure ((floor((fromIntegral len :: Double) * a) :: Int) `mod` len)

hull_hash_edge
    :: HasCallStack
    => HullST s
    -> Vec2
    -> Int
    -> ST s ()
hull_hash_edge hull p i = do
    key <- hull_hash_key hull p
    VM.write (_hash hull) key i

hull_find_visible_edge
    :: HasCallStack
    => HullST s
    -> Vec2        -- ^ Newly inserted point
    -> Vector Vec2 -- ^ Input points
    -> ST s (Int, Bool)
hull_find_visible_edge hull p points = do
    startRef <- newSTRef 0
    key <- hull_hash_key hull p
    let len = _hashLen hull

    -- // find a visible edge on the convex hull using edge hash
    do  let loop j | j >= len = pure () -- end of loop
            loop j = do
                start <- VM.read (_hash hull) ((key+j) `mod` len)
                writeSTRef startRef start
                if start /= tEMPTY
                    then do
                        next <- VM.read (_next hull) start
                        if next /= tEMPTY
                            then pure () -- break: we found a good start
                            else loop (j+1)
                    else loop (j+1)
        loop 0

    start <- VM.read (_prev hull) =<< readSTRef startRef
    eRef <- newSTRef start

    fix $ \loop -> do
        e <- readSTRef eRef
        next_e <- VM.read (_next hull) e
        if orientation p (points!e) (points!next_e) /= Clockwise -- Rust source: <=0, equivalent to degenerate or counterclockwise
            then do
                writeSTRef eRef next_e
                if next_e == start
                    then pure (tEMPTY, False)
                    else loop
            else pure (e, e == start)

-- | The seed triangle is the first triangle, located at the center of the input
-- points. Other triangles will be grown around the seed.
findSeedTriangle :: HasCallStack => Vector Vec2 -> Maybe (Int, Int, Int)
findSeedTriangle points
    -- Make sure there is some initial triangle at all. Fails when the input starts
    -- with two identical points even though the rest might be legal, too bad for that user.
    | V.length points < 3 = Nothing
    | isInfinite (circumradiusSquare (points!0) (points!1) (points!2)) = Nothing
findSeedTriangle points = Just $

        -- Pick the first point close to the center
    let bboxCenter = boundingBoxCenter points
        i0 = V.minIndexBy (\p q -> comparing (distSquare bboxCenter) p q) points
        p0 = points!i0

        -- Find a second point close to the first
        (i1,p1) = V.minimumBy (\(_,p) (_,q) -> comparing (distSquare p0) p q) . V.filter (\(i,_) -> i /= i0) $ V.indexed points

        -- Find the third point which forms the smallest circumcircle with the first two
        (i2,p2) = V.minimumBy (\(_,p) (_,q) -> comparing (circumradiusSquare p0 p1) p q) . V.filter (\(i,_) -> i `notElem` [i0,i1]) . V.indexed $ points

    in if orientation p0 p1 p2 == Clockwise
        then (i0, i2, i1) -- Flip two points to ensure counterclockwise orientation
        else (i0, i1, i2)

-- | Inplace sort of the input by distance.
--
-- __Rust source:__ @sortf@
sortf :: STVector s (a, Double) -> ST s ()
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
--
-- __Rust source:__ @triangulate@
triangulateMut :: HasCallStack => Vector Vec2 -> ST s (TriangulationST s, HullST s)
triangulateMut points = do
    case findSeedTriangle points of
        Nothing -> error "Can’t find a seed triangle, and handle_collinear_points is not implemented" -- TODO!
        Just seed_triangle -> triangulate_for_real seed_triangle
  where
    triangulate_for_real :: (Int, Int, Int) -> ST s (TriangulationST s, HullST s)
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
                else do

                    -- // find a visible edge on the convex hull using edge hash
                    (e0, walk_back) <- hull_find_visible_edge hull p points

                    if e0 == tEMPTY
                        then pure () -- // likely a near-duplicate point; skip it
                        else addPoint hull tgl e0 walk_back i p

        pure (tgl, hull)

    addPoint
        :: HullST s
        -> TriangulationST s
        -> Int  -- ^ Visible edge from p
        -> Bool -- ^ walk_back
        -> Int  -- ^ Index of p
        -> Vec2 -- ^ Coordinates of p
        -> ST s ()
    addPoint hull tgl e0 walk_back i p = do

        eRef <- newSTRef e0
        -- // add the first triangle from the point
        do
            e <- readSTRef eRef
            t <- do
                hull_next_e <- VM.read (_next hull) e
                hull_tri_e <- VM.read (_tri hull) e
                triangulation_add_triangle tgl e i hull_next_e tEMPTY tEMPTY hull_tri_e

            -- // recursively flip triangles from the point until they satisfy the Delaunay condition
            VM.write (_tri hull) i =<< triangulation_legalize tgl (t+2) points hull
            VM.write (_tri hull) e t -- // keep track of boundary triangles on the hull

        -- // walk forward through the hull, adding more triangles and flipping recursively
        nRef <- do
            e <- readSTRef eRef
            hull_next_e <- VM.read (_next hull) e
            newSTRef hull_next_e
        fix $ \loop -> do
            n <- readSTRef nRef
            q <- VM.read (_next hull) n
            if orientation p (points!n) (points!q) /= Clockwise -- Rust source: <=0, equivalent to degenerate-or-counterclockwise
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
        when walk_back $ do
            fix $ \loop -> do
                e <- readSTRef eRef
                q <- VM.read (_prev hull) e
                if orientation p (points!q) (points!e) /= Clockwise -- Rust source: <=0, equivalent to degenerate-or-counterclockwise
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

        -- // update the hull indices
        e <- readSTRef eRef
        n <- readSTRef nRef
        VM.write (_prev hull) i e
        VM.write (_next hull) i n
        VM.write (_prev hull) n i
        VM.write (_next hull) e i
        writeSTRef (_start hull) e

        -- // expose hull as a vector of point indices
        -- ==> This is done in 'freezeHull' outside of this function.

        -- // save the two new edges in the hash table
        hull_hash_edge hull p i
        hull_hash_edge hull (points!e) e

-- | Create the list of hull points based on the previously calculated hull data.
freezeHull :: HullST s -> ST s (Vector Int)
freezeHull hull = do
    hull_start <- readSTRef (_start hull)
    V.unfoldrM
        -- We add an 'isFirst' flag here so we can distinguish the cases
        --    1. We start with e == hull_start but want to continue
        --    2. We have e == hull_start after circling the hull and want to terminate
        (\(e, isFirst) -> do
            next_e <- VM.read (_next hull) e -- prev/next switch hull orientation here. Pick whatever you like.
            if not isFirst && e == hull_start
                then pure Nothing
                else pure (Just (e, (next_e, False)))
        )
        (hull_start, True)

-- | User-facing raw Delaunay triangulation data. Build the API based on this and
-- 'triangulate'. See 'TriangulationST' for documentation of the fields.
data TriangulationRaw = TriangulationRaw
    { _triangles :: Vector Int
    , _halfedges :: Vector Int
    , _convexHull :: Vector Int
    } deriving (Eq, Ord, Show)

instance NFData TriangulationRaw where
    rnf (TriangulationRaw a b c) = rnf (a,b,c)

-- | NB: unsafeFreeze is safe here, since this module encapsulates the mutable
-- triangulation along with 'triangulation_new'.
freezeTriangulation :: HasCallStack => TriangulationST s -> HullST s -> ST s TriangulationRaw
freezeTriangulation tglMut hullMut = do
    trianglesLen <- readSTRef (__trianglesLen tglMut)
    triangles <- V.unsafeFreeze (VM.take trianglesLen (__triangles tglMut))
    halfedges <- V.unsafeFreeze (VM.take trianglesLen (__halfedges tglMut))

    hull <- freezeHull hullMut
    pure TriangulationRaw
        { _triangles = triangles
        , _halfedges = halfedges
        , _convexHull = hull
        }

-- | Main result from the raw internal module: a frozen-but-raw triangulation, to
-- be boxed nicely as vectors of polygons etc.
triangulate :: HasCallStack => Vector Vec2 -> TriangulationRaw
triangulate points = runST $ do
    (tglMut, hullMut) <- triangulateMut points
    freezeTriangulation tglMut hullMut
