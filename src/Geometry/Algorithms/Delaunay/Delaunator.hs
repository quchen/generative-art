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
import           Data.Function
import           Data.Ord
import           Data.STRef
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as VM
import           Geometry.Core


-- | Near-duplicate points (where both @x@ and @y@ only differ within this value)
-- will not be included in the triangulation for robustness.
epsilon :: Double
epsilon = 2*ieee_float64_epsilon
  where
    -- Got this by printing f64::EPSILON in a Rust repl.it.
    -- Couldn’t find it in Haskell’s base.
    ieee_float64_epsilon = 2.2204460492503131e-16

dist2 :: Vec2 -> Vec2 -> Double
dist2 x y = normSquare (x -. y)

-- | TODO TEST because sign flips and inverted screen coordinates *rage*
--
-- * Returns a negative value if @x@, @q@ and @r@ occur in counterclockwise order
--   (@r@ is to the left of the directed line @x@ --> @q@)
-- * Returns a positive value if they occur in clockwise order
--   (@r@ is to the right of the directed line @x@ --> @q@)
-- * Returns zero is they are collinear
orient :: Vec2 -> Vec2 -> Vec2 -> Double
orient x q r = cross lineQ lineR
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
    in dx <= epsilon && dy <= epsilon

-- Type synonym so copy+paste is less ambiguous from the Rust code
type USize = Int

-- Represents the area outside of the triangulation. Halfedges on the convex hull
-- (which don't have an adjacent halfedge) will have this value.
triangulation_EMPTY :: USize
triangulation_EMPTY = maxBound

nextHalfedge, prevHalfedge :: USize -> USize
nextHalfedge i = if mod i 3 == 2 then i-2 else i+1
prevHalfedge i = if mod i 3 == 0 then i+2 else i-1

data Triangulation s = Triangulation
    { _triangles :: STVector s USize
    -- ^ A vector of point indices where each triple represents a Delaunay triangle.
    -- All triangles are directed counter-clockwise.
    , _trianglesLen :: STRef s USize

    , _halfedges :: STVector s USize
    -- ^ A vector of adjacent halfedge indices that allows traversing the triangulation graph.
    --
    -- `i`-th half-edge in the array corresponds to vertex `triangles[i]`
    -- the half-edge is coming from. `halfedges[i]` is the index of a twin half-edge
    -- in an adjacent triangle (or `EMPTY` for outer half-edges on the convex hull).

    , _hull :: STVector s USize
    -- ^ A vector of indices that reference points on the convex hull of the triangulation,
    -- counter-clockwise.
    }

{-# DEPRECATED newVectorWithGoodErrorMessages "Use VM.unsafeNew instead once the code works" #-}
newVectorWithGoodErrorMessages :: String -> Int -> ST s (STVector s a)
newVectorWithGoodErrorMessages name n
    | libTested = VM.unsafeNew n
    | otherwise = VM.generate n (\i -> error ("Uninitialized element " ++ show i ++ " in vector " ++ name))
  where
    libTested = False

triangulation_new :: USize -> ST s (Triangulation s)
triangulation_new n = do
    let maxTriangles = if n > 2 then 2*n-5 else 0
    triangles <- newVectorWithGoodErrorMessages "triangles" (maxTriangles*3)
    trianglesLen <- newSTRef 0
    halfedges <- newVectorWithGoodErrorMessages "halfedges" (maxTriangles*3)
    hull <- newVectorWithGoodErrorMessages "halfedges, probably oversized" (maxTriangles*3)
    pure Triangulation
        { _triangles = triangles
        , _trianglesLen = trianglesLen
        , _halfedges = halfedges
        , _hull = hull
        }

triangulation_len :: Triangulation s -> ST s USize
triangulation_len Triangulation{_trianglesLen = lenRef} = readSTRef lenRef

triangulation_is_empty :: Triangulation s -> ST s Bool
triangulation_is_empty tri = fmap (== 0) (triangulation_len tri)

-- | Add a new triangle to the triangulation; report the old (!) size (why, Rust source?!).
triangulation_add_triangle
    :: Triangulation s
    -> USize -- ^ Corner i0
    -> USize -- ^ Corner i1
    -> USize -- ^ Corner i2
    -> USize -- ^ Halfedge a
    -> USize -- ^ Halfedge b
    -> USize -- ^ Halfedge c
    -> ST s USize
triangulation_add_triangle tgl i0 i1 i2 a b c = do
    t <- triangulation_len tgl

    let triangles = _triangles tgl
    VM.write triangles  t    i0
    VM.write triangles (t+1) i1
    VM.write triangles (t+2) i2
    modifySTRef' (_trianglesLen tgl) (+3)

    let halfedges = _halfedges tgl
    VM.write halfedges  t    a
    VM.write halfedges (t+1) b
    VM.write halfedges (t+2) c

    when (a /= triangulation_EMPTY) (VM.write halfedges a  t   )
    when (b /= triangulation_EMPTY) (VM.write halfedges b (t+1))
    when (c /= triangulation_EMPTY) (VM.write halfedges c (t+2))

    pure t

legalize :: Triangulation s -> USize -> Vector Vec2 -> Hull s -> ST s USize
legalize tgl a points hull = do
    let self_triangles = _triangles tgl
    let self_halfedges = _halfedges tgl

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

    case b == triangulation_EMPTY of
        True -> pure ar
        False -> do
            let al = nextHalfedge a;
                bl = prevHalfedge b;

            p0 <- VM.read self_triangles ar
            pr <- VM.read self_triangles a
            pl <- VM.read self_triangles al
            p1 <- VM.read self_triangles bl

            let illegal = inCircle (points V.! p0) (points V.! pr) (points V.! pl) (points V.! p1)
            case illegal of
                True -> do
                    VM.write self_triangles a p1
                    VM.write self_triangles a p0

                    hbl <- VM.read self_triangles bl
                    har <- VM.read self_triangles ar

                    -- edge swapped on the other side of the hull (rare); fix the halfedge reference
                    when (hbl == triangulation_EMPTY) $ do
                        eRef <- newSTRef (_start hull)
                        fix $ \loop -> do
                            e <- readSTRef eRef
                            tri_e <- VM.read (_tri hull) e
                            if tri_e == bl
                                then do
                                    VM.write (_tri hull) e a
                                    pure () -- break
                                else do
                                    prev_e <- VM.read (_prev hull) e
                                    let e' = prev_e
                                    writeSTRef eRef e'
                                    if e == _start hull
                                        then pure () -- break
                                        else loop

                    VM.write self_halfedges a hbl
                    VM.write self_halfedges b har
                    VM.write self_halfedges ar bl

                    when (hbl /= triangulation_EMPTY) (VM.write self_halfedges hbl a)
                    when (har /= triangulation_EMPTY) (VM.write self_halfedges har b)
                    when (bl /= triangulation_EMPTY) (VM.write self_halfedges bl ar)

                    let br = nextHalfedge b
                    _ <- legalize tgl a points hull
                    legalize tgl br points hull

                False -> pure ar

data Hull s = Hull
    { _prev :: STVector s USize
    , _next :: STVector s USize
    , _tri :: STVector s USize
    , _hash :: STVector s USize
    , _hashLen :: Int
    , _start :: USize
    , _center :: Vec2
    }

hull_new :: USize -> Vec2 -> USize -> USize -> USize -> Vector Vec2 -> ST s (Hull s)
hull_new n center i0 i1 i2 points = do
    let hash_len :: USize
        hash_len = floor (sqrt (fromIntegral n))

    prev <- VM.replicate n 0
    next <- VM.replicate n 0
    tri <- VM.replicate n 0
    hash <- VM.replicate hash_len triangulation_EMPTY

    let hull = Hull
            { _prev = prev
            , _next = next
            , _tri = tri
            , _hash = hash
            , _hashLen = hash_len
            , _start = i0
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

    hull_hash_edge hull (points V.! i0) i0
    hull_hash_edge hull (points V.! i1) i1
    hull_hash_edge hull (points V.! i2) i2

    pure hull

hull_hash_key :: Hull s -> Vec2 -> ST s USize
hull_hash_key hull p = do
    let center = _center hull
        Vec2 dx dy = p -. center

        pp = dx / (abs dx + abs dy);
        a = (if dy > 0.0 then 3.0 - pp else 1.0 + pp) / 4.0; -- [0..1] --- <- I don’t understand this comment in the Rust source

    let len = _hashLen hull
    pure ((floor((fromIntegral len :: Double) * a) :: USize) `mod` len)

hull_hash_edge :: Hull s -> Vec2 -> USize -> ST s ()
hull_hash_edge hull p i = do
    key <- hull_hash_key hull p
    VM.write (_hash hull) key i

hull_find_visible_edge :: Hull s -> Vec2 -> Vector Vec2 -> ST s (USize, Bool)
hull_find_visible_edge hull p points = do
    startRef <- newSTRef 0
    key <- hull_hash_key hull p
    let len = _hashLen hull

    flip fix 0 $ \loop j -> do
        start <- VM.read (_hash hull) ((key+j) `mod` len)
        writeSTRef startRef start
        next <- VM.read (_next hull) start
        if start /= triangulation_EMPTY && next /= triangulation_EMPTY
            then pure () -- break
            else loop (j+1)

    start <- readSTRef startRef
    prev_start <- VM.read (_prev hull) start
    eRef <- newSTRef prev_start

    earlyReturnHack <- fix $ \loop -> do
        e <- readSTRef eRef
        next_e <- VM.read (_next hull) e
        if orient p (points V.! e) (points V.! next_e) <= 0
            then do
                writeSTRef eRef next_e
                e' <- readSTRef eRef
                if e' == start
                    then pure (Just (triangulation_EMPTY, False))
                    else loop
            else pure Nothing -- exit while loop

    case earlyReturnHack of
        Just earlyReturn -> pure earlyReturn
        Nothing -> do
            e <- readSTRef eRef
            pure (e, e == start)

calc_bbox_center :: Vector Vec2 -> Vec2
calc_bbox_center = boundingBoxCenter

find_closest_point :: Vector Vec2 -> Vec2 -> Maybe USize
find_closest_point points p0
    | V.null points = Nothing
    | otherwise = Just (V.minIndexBy (\x y -> comparing normSquare (x -. p0) (y -. p0)) points)

find_seed_triangle :: Vector Vec2
-- fn find_seed_triangle(points: &[Point]) -> Option<(usize, usize, usize)> {
--     // pick a seed point close to the center
--     let bbox_center = calc_bbox_center(points);
--     let i0 = find_closest_point(points, &bbox_center)?;
--     let p0 = &points[i0];
--
--     // find the point closest to the seed
--     let i1 = find_closest_point(points, p0)?;
--     let p1 = &points[i1];
--
--     // find the third point which forms the smallest circumcircle with the first two
--     let mut min_radius = f64::INFINITY;
--     let mut i2: usize = 0;
--     for (i, p) in points.iter().enumerate() {
--         if i == i0 || i == i1 {
--             continue;
--         }
--         let r = p0.circumradius2(p1, p);
--         if r < min_radius {
--             i2 = i;
--             min_radius = r;
--         }
--     }
--
--     if min_radius == f64::INFINITY {
--         None
--     } else {
--         // swap the order of the seed points for counter-clockwise orientation
--         Some(if p0.orient(p1, &points[i2]) > 0. {
--             (i0, i2, i1)
--         } else {
--             (i0, i1, i2)
--         })
--     }
-- }
--
-- fn sortf(f: &mut [(usize, f64)]) {
--     f.sort_unstable_by(|&(_, da), &(_, db)| da.partial_cmp(&db).unwrap_or(Ordering::Equal));
-- }
--
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
--
-- /// Triangulate a set of 2D points.
-- /// Returns the triangulation for the input points.
-- /// For the degenerated case when all points are collinear, returns an empty triangulation where all points are in the hull.
-- pub fn triangulate(points: &[Point]) -> Triangulation {
--     let seed_triangle = find_seed_triangle(points);
--     if seed_triangle.is_none() {
--         return handle_collinear_points(points);
--     }
--
--     let n = points.len();
--     let (i0, i1, i2) =
--         seed_triangle.expect("At this stage, points are guaranteed to yeild a seed triangle");
--     let center = points[i0].circumcenter(&points[i1], &points[i2]);
--
--     let mut triangulation = Triangulation::new(n);
--     triangulation.triangulation_add_triangle(i0, i1, i2, EMPTY, EMPTY, EMPTY);
--
--     // sort the points by distance from the seed triangle circumcenter
--     let mut dists: Vec<_> = points
--         .iter()
--         .enumerate()
--         .map(|(i, point)| (i, center.dist2(point)))
--         .collect();
--
--     sortf(&mut dists);
--
--     let mut hull = Hull::new(n, center, i0, i1, i2, points);
--
--     for (k, &(i, _)) in dists.iter().enumerate() {
--         let p = &points[i];
--
--         // skip near-duplicates
--         if k > 0 && p.nearly_equals(&points[dists[k - 1].0]) {
--             continue;
--         }
--         // skip seed triangle points
--         if i == i0 || i == i1 || i == i2 {
--             continue;
--         }
--
--         // find a visible edge on the convex hull using edge hash
--         let (mut e, walk_back) = hull.find_visible_edge(p, points);
--         if e == EMPTY {
--             continue; // likely a near-duplicate point; skip it
--         }
--
--         // add the first triangle from the point
--         let t = triangulation.triangulation_add_triangle(e, i, hull.next[e], EMPTY, EMPTY, hull.tri[e]);
--
--         // recursively flip triangles from the point until they satisfy the Delaunay condition
--         hull.tri[i] = triangulation.legalize(t + 2, points, &mut hull);
--         hull.tri[e] = t; // keep track of boundary triangles on the hull
--
--         // walk forward through the hull, adding more triangles and flipping recursively
--         let mut n = hull.next[e];
--         loop {
--             let q = hull.next[n];
--             if p.orient(&points[n], &points[q]) <= 0. {
--                 break;
--             }
--             let t = triangulation.triangulation_add_triangle(n, i, q, hull.tri[i], EMPTY, hull.tri[n]);
--             hull.tri[i] = triangulation.legalize(t + 2, points, &mut hull);
--             hull.next[n] = EMPTY; // mark as removed
--             n = q;
--         }
--
--         // walk backward from the other side, adding more triangles and flipping
--         if walk_back {
--             loop {
--                 let q = hull.prev[e];
--                 if p.orient(&points[q], &points[e]) <= 0. {
--                     break;
--                 }
--                 let t = triangulation.triangulation_add_triangle(q, i, e, EMPTY, hull.tri[e], hull.tri[q]);
--                 triangulation.legalize(t + 2, points, &mut hull);
--                 hull.tri[q] = t;
--                 hull.next[e] = EMPTY; // mark as removed
--                 e = q;
--             }
--         }
--
--         // update the hull indices
--         hull.prev[i] = e;
--         hull.next[i] = n;
--         hull.prev[n] = i;
--         hull.next[e] = i;
--         hull.start = e;
--
--         // save the two new edges in the hash table
--         hull.hash_edge(p, i);
--         hull.hash_edge(&points[e], e);
--     }
--
--     // expose hull as a vector of point indices
--     let mut e = hull.start;
--     loop {
--         triangulation.hull.push(e);
--         e = hull.next[e];
--         if e == hull.start {
--             break;
--         }
--     }
--
--     triangulation.triangles.shrink_to_fit();
--     triangulation.halfedges.shrink_to_fit();
--
--     triangulation
-- }
--
-- #[cfg(feature = "std")]
-- #[inline]
-- fn f64_abs(f: f64) -> f64 {
--     f.abs()
-- }
--
-- #[cfg(not(feature = "std"))]
-- #[inline]
-- fn f64_abs(f: f64) -> f64 {
--     const SIGN_BIT: u64 = 1 << 63;
--     f64::from_bits(f64::to_bits(f) & !SIGN_BIT)
-- }
--
-- #[cfg(feature = "std")]
-- #[inline]
-- fn f64_floor(f: f64) -> f64 {
--     f.floor()
-- }
--
-- #[cfg(not(feature = "std"))]
-- #[inline]
-- fn f64_floor(f: f64) -> f64 {
--     let mut res = (f as i64) as f64;
--     if res > f {
--         res -= 1.0;
--     }
--     res as f64
-- }
--
-- #[cfg(feature = "std")]
-- #[inline]
-- fn f64_sqrt(f: f64) -> f64 {
--     f.sqrt()
-- }
--
-- #[cfg(not(feature = "std"))]
-- #[inline]
-- fn f64_sqrt(f: f64) -> f64 {
--     if f < 2.0 {
--         return f;
--     };
--
--     let sc = f64_sqrt(f / 4.0) * 2.0;
--     let lc = sc + 1.0;
--
--     if lc * lc > f {
--         sc
--     } else {
--         lc
--     }
-- }
