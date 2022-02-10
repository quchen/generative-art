{-# LANGUAGE ScopedTypeVariables #-}

module Geometry.Trajectory (
      pointOnTrajectory

    , simplifyTrajectory
    , simplifyTrajectoryBy

    , simplifyTrajectoryVW
    , simplifyTrajectoryVWBy

    , simplifyTrajectoryRadial
    , simplifyTrajectoryRadialBy

    , reassembleLines
) where



import           Data.Foldable
import           Data.Heap            (Entry (..), Heap)
import qualified Data.Heap            as H
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Ord
import           Data.Sequence        (Seq, (|>))
import qualified Data.Sequence        as Seq
import           Data.Sequential
import           Data.Vector          (Vector, (!))
import qualified Data.Vector          as V
import           GHC.Stack            (HasCallStack)
import           Geometry.Core
import           Geometry.LookupTable
import           Prelude              hiding (lines)



-- | Build a lookup table from arc length to the line that we’re on at that arc
-- length.
trajectoryLut :: [Vec2] -> VectorLookupTable Double Line
trajectoryLut = VectorLookupTable . V.fromList . go 0 . pairLines
  where
    pairLines :: [Vec2] -> [Line]
    pairLines xs = zipWith Line xs (tail xs)

    go :: Double -> [Line] -> [(Double, Line)]
    go !_ [] = []
    go currentDistance (currentLine:rest)
       = let newPosition = currentDistance +. lineLength currentLine
         in (currentDistance, currentLine) : go newPosition rest

-- | Walk a certain 'Distance' on a trajectory defined by its points.
--
-- This caches the internal LUT when partially applied, so that the following will
-- only compute it once for repeated lookups:
--
-- @
-- let goto = 'pointOnTrajectory' […]
-- 'print' [goto ('Distance' d) | d <- [0, 0.1 .. 5]]
-- @
pointOnTrajectory :: Sequential list => list Vec2 -> Double -> Vec2
pointOnTrajectory points
  = let lut = trajectoryLut (toList points)
    in \dist ->
        let (start, line) = lookupBiasLower lut dist
            distLeft = dist -. start
        in moveAlongLine line distLeft


-- | Simplify a path by dropping unnecessary points, using the the
-- [Ramer-Douglas-Peucker algorithm]
-- (https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm).
-- The larger the tolerance distance, the simpler the result will be.
--
-- This is very useful in conjunction with 'bezierSmoothen': first drop the
-- redundancies, then smoothen using Bezier curves again, to yield a result
-- visually similar to the original data, but with a much smaller data footprint
-- (SVGs can become huge!).
--
-- If your trajectory contains more than just the points you want to simplify on,
-- use 'simplifyTrajectoryBy'.
--
-- <<docs/interpolation/3_simplify_path_rdp.svg>>
simplifyTrajectory
    :: Sequential vector
    => Double      -- ^ Discard points closer than \(\varepsilon\) to the connecting line of two points. Larger values yield simpler results.
    -> vector Vec2 -- ^ Trajectory
    -> Vector Vec2 -- ^ Simplified trajectory
simplifyTrajectory epsilon = simplifyTrajectoryBy epsilon id

-- | 'simplifyTrajectory', but allows specifying a function for how to extract the points
-- to base simplifying on from the input.
--
-- This is useful when your trajectory contains metadata, such as the velocity at
-- each point.
simplifyTrajectoryBy
    :: Sequential vector
    => Double      -- ^ Discard points closer than \(\varepsilon\) to the connecting line of two points. Larger values yield simpler results.
    -> (a -> Vec2) -- ^ Extract the relevant 'Vec2' to simplify on
    -> vector a    -- ^ Trajectory
    -> Vector a    -- ^ Simplified trajectory
simplifyTrajectoryBy epsilon vec2in = go . toVector
  where
    go points | V.length points <= 2 = points
    go points
      = let start = V.head points
            end = V.last points
            isCyclic = vec2in start == vec2in end

            (dMax, iMax)
                | isCyclic = V.maximumBy (comparing fst) (V.imap (\i p -> (norm (vec2in p -. vec2in start), i)) points)
                | otherwise =
                    let connectingLine = Line (vec2in start) (vec2in end)
                    in V.maximumBy (comparing fst) (V.imap (\i p -> (distanceFromLine (vec2in p) connectingLine, i)) points)
        in if dMax <= epsilon
            -- Points are all too close: remove them
            then V.fromList [start, end]
            -- Points far enough away: recurse
            else let before = V.take (iMax+1) points -- +1 so this includes the iMax point
                     after = V.drop iMax points
                 in go before <> V.drop 1 (go after) -- Don’t add the middle twice

-- | __Warning:__ Due to being poorly implemented, this is currently quite slow, is
-- probably at least quadratic (see code comment @BOTTLENECK@).
--
-- Simplify a path by dropping unnecessary points using the the
-- [Visvalingam-Whyatt algorithm]
-- (https://en.wikipedia.org/wiki/Visvalingam%E2%80%93Whyatt_algorithm). The larger
-- the cutoff parameter, the simpler the result will be.
--
-- This is very useful in conjunction with 'bezierSmoothen': first drop the
-- redundancies, then smoothen using Bezier curves again, to yield a result
-- visually similar to the original data, but with a much smaller data footprint
-- (SVGs can become huge!).
--
-- If your trajectory contains more than just the points you want to simplify on,
-- use 'simplifyTrajectoryVWBy'.
--
-- <<docs/interpolation/3_simplify_path_vw.svg>>
simplifyTrajectoryVW
    :: Sequential vector
    => Double      -- ^ Cutoff parameter. We remove points that span triangles smaller than this. Larger values yield simpler results.
    -> vector Vec2 -- ^ Trajectory
    -> Vector Vec2 -- ^ Simplified trajectory
simplifyTrajectoryVW areaCutoff = simplifyTrajectoryVWBy areaCutoff id . toVector

-- | 'simplifyTrajectoryVWBy', but allows specifying a function for how to extract the points
-- to base simplifying on from the input.
--
-- This is useful when your trajectory contains metadata, such as the velocity at
-- each point.
simplifyTrajectoryVWBy
    :: forall a vector.
       (Ord a, Sequential vector)
    => Double      -- ^ Cutoff parameter. We remove points that span triangles smaller than this. Larger values yield simpler results.
    -> (a -> Vec2) -- ^ Extract the relevant 'Vec2' to simplify on
    -> vector a    -- ^ Trajectory
    -> Vector a    -- ^ Simplified trajectory
simplifyTrajectoryVWBy areaCutoff vec2in trajectorySequence =
    let trajectory = toVector trajectorySequence
        isCyclic = vec2in (V.head trajectory) == vec2in (V.last trajectory)
        areaHeap :: Heap (Entry Double Vec2)
        areaHeap =
            let vecs = fmap vec2in trajectory
                vecs' = vecs <> V.take 2 (V.tail vecs)
                areas
                    | isCyclic  = V.zipWith3 (\a b c -> Entry (triangleArea a b c) b) (V.drop 1 vecs) (V.drop 2 vecs') (V.drop 3 vecs')
                    | otherwise = V.zipWith3 (\a b c -> Entry (triangleArea a b c) b) (         vecs) (V.drop 1 vecs ) (V.drop 2 vecs )
                heap = H.fromList (V.toList areas)
            in heap

        neighbourMap :: Map Vec2 (LeftRightBoth a)
        neighbourMap
            | isCyclic =
                let trajectory' = trajectory <> V.take 2 trajectory
                in M.fromList (V.toList (V.zipWith3 (\left center right -> (vec2in center, NBoth left right)) trajectory (V.tail trajectory') (V.tail (V.tail trajectory'))))
            | otherwise = M.fromList
                ( (vec2in (V.head trajectory), NRight (trajectory!1))
                : (vec2in (V.last trajectory), NLeft (trajectory!(V.length trajectory-2)))
                : V.toList (V.zipWith3 (\left center right -> (vec2in center, NBoth left right)) trajectory (V.tail trajectory) (V.tail (V.tail trajectory)))
                )

        deleteTrianglesBySize :: Heap (Entry Double Vec2) -> Map Vec2 (LeftRightBoth a) -> Map Vec2 (LeftRightBoth a)
        deleteTrianglesBySize heap neighbours = case H.viewMin heap of
            _ | not isCyclic && (H.size areaHeap /= M.size neighbourMap - 2) -> bugErrorVW $
                "For linear trajectories, the heap should contain the neighbour map’s entries, minus the first+last points."
                ++ show (H.size areaHeap, M.size neighbourMap)
            _ | isCyclic && (H.size areaHeap /= M.size neighbourMap) -> bugErrorVW $
                "For cyclic trajectories, the heap should contain the neighbour map’s entries."
                ++ show (H.size areaHeap, M.size neighbourMap)
            Nothing
                -- Heap is empty, so there are no points left. We could just return mempty here,
                -- but for better testing we check for consistency.
                | M.size neighbours <= 2 -> neighbours
                | otherwise -> bugErrorVW ("Heap is empty, but neighbour map contains more than two elements (i.e. intermeidate points are left unvisited), namely: " ++ show (M.size neighbours))
            Just (Entry area point, heap')
                | area > areaCutoff -> neighbours -- The smallest triangle is too large, terminate
                | M.size neighbours == 1 -> neighbours -- We reduced the trajectory to a single point, terminate
                | otherwise ->
                    let newNeighbours = deleteFromDirectionalNeighbourMap vec2in point neighbours
                        -- BOTTLENECK: this filter takes up most of the computation time. Since it’s
                        -- being run quite often, and compared to the original algorithm which allows modifying
                        -- priorities (here: area) within the heap with delete/decreasePrio functions,
                        -- I suspect this adds a factor of n to the runtime, making this at least O(n²), if
                        -- not O(n²•log(n)).
                        heapRemove xs = H.filter (\(Entry _ x) -> x `notElem` xs)
                        heapUpdateL l = (case M.lookup l newNeighbours of
                            Just (NBoth ll lr) -> H.insert (Entry (triangleArea (vec2in ll) l (vec2in lr)) l)
                            _other -> id)
                        heapUpdateR r = case M.lookup r newNeighbours of
                            Just (NBoth rl rr) -> H.insert (Entry (triangleArea (vec2in rl) r (vec2in rr)) r)
                            _other -> id
                        newHeap = case (fmap.fmap) vec2in (M.lookup point neighbours) of
                            Nothing -> bugErrorVW "Current point does not have any neighbours"
                            Just (NLeft l) -> (heapUpdateL l . heapRemove [l]) heap'
                            Just (NBoth l r) -> (heapUpdateL l . heapUpdateR r . heapRemove [l, r]) heap'
                            Just (NRight r) -> (heapUpdateR r . heapRemove [r]) heap'
                    in deleteTrianglesBySize newHeap newNeighbours

        reconstructTrajectory :: a -> a -> Map Vec2 (LeftRightBoth a) -> [a]
        reconstructTrajectory startingPoint  currentPoint nMap = currentPoint : case M.lookup (vec2in currentPoint) nMap of
            Nothing
                | isCyclic -> [startingPoint]
                | otherwise -> bugErrorVW "The last entry should have a left neighbour instead of being not there"
            Just (NLeft _)
                | M.size nMap == 1 -> [] -- The last entry only has a left neighbour
                | otherwise -> bugErrorVW ("Neighbour map is not sequential, " ++ show (M.size nMap) ++ " entries remain")
            Just (NRight r) -> reconstructTrajectory startingPoint r (M.delete (vec2in currentPoint) nMap)
            Just (NBoth _l r) -> reconstructTrajectory startingPoint r (M.delete (vec2in currentPoint) nMap)
    in (V.fromList . reconstructTrajectory (V.head trajectory) (V.head trajectory) . deleteTrianglesBySize areaHeap) neighbourMap

bugErrorVW :: HasCallStack => String -> error
bugErrorVW str = bugError ("simplifyVW: " <> str)

-- | Neighbours to the left and/or right.
data LeftRightBoth ty
    = NLeft ty
    | NBoth ty ty
    | NRight ty
    deriving (Eq, Ord, Show)

instance Functor LeftRightBoth where
    fmap f (NLeft x) = NLeft (f x)
    fmap f (NBoth x y) = NBoth (f x) (f y)
    fmap f (NRight x) = NRight (f x)

deleteFromDirectionalNeighbourMap :: Ord k => (lr -> k) -> k -> Map k (LeftRightBoth lr) -> Map k (LeftRightBoth lr)
deleteFromDirectionalNeighbourMap vec2in point neighbours =
  case M.lookup point neighbours of
    Nothing -> bugErrorVW "Bad neighbour map: expected point not present"
    Just (NBoth leftOfPoint rightOfPoint) ->
          M.delete point
        . updateRightNeighbour (vec2in rightOfPoint) (Just leftOfPoint)
        . updateLeftNeighbour (vec2in leftOfPoint) (Just rightOfPoint)
        $ neighbours
    Just (NLeft leftOfPoint) ->
          M.delete point
        . updateLeftNeighbour (vec2in leftOfPoint) Nothing
        $ neighbours
    Just (NRight rightOfPoint) ->
          M.delete point
        . updateRightNeighbour (vec2in rightOfPoint) Nothing
        $ neighbours

-- | When deleting a node, we have to tell its left neighbour that its right
-- neighbour is now the deleted node’s right neighbour.
updateLeftNeighbour
    :: Ord k
    => k        -- ^ Subject: the node whose neighbour should be updated
    -> Maybe lr -- ^ New right neighbour (of the node to the left of the subject), if any.
    -> Map k (LeftRightBoth lr)
    -> Map k (LeftRightBoth lr)
updateLeftNeighbour subject newRight neighbours = case M.lookup subject neighbours of
    Nothing -> bugErrorVW "Bad neighbour map! [Error tag: B]"
    Just (NLeft _) -> bugErrorVW "Bad neighbour map: a left neighbour always has a right neighbour (the node we’re coming from)! [Error tag: C]"
    Just (NBoth l _r) -> case newRight of
        Just newRight' -> M.insert subject (NBoth l newRight') neighbours
        Nothing -> M.insert subject (NLeft l) neighbours
    Just (NRight _r) -> case newRight of
        Just newRight' -> M.insert subject (NRight newRight') neighbours
        Nothing -> M.delete subject neighbours

-- | Like 'updateLeftNeighbour', but for the other side.
updateRightNeighbour
    :: Ord k
    => k
    -> Maybe lr
    -> Map k (LeftRightBoth lr)
    -> Map k (LeftRightBoth lr)
updateRightNeighbour subject newLeft neighbours = case M.lookup subject neighbours of
    Nothing -> bugErrorVW "Bad neighbour map! [Error tag: D]"
    Just (NLeft _l) -> case newLeft of
        Just newLeft' -> M.insert subject (NLeft newLeft') neighbours
        Nothing -> M.delete subject neighbours
    Just (NBoth _l r) -> case newLeft of
        Just newLeft' -> M.insert subject (NBoth newLeft' r) neighbours
        Nothing -> M.insert subject (NRight r) neighbours
    Just (NRight _) -> bugErrorVW "Bad neighbour map: a right neighbour always has a left neighbour (the node we’re coming from)! [Error tag: E]"

-- | Area of a triangle, defined by its corners.
triangleArea :: Vec2 -> Vec2 -> Vec2 -> Double
triangleArea left center right = abs (det (left -. center) (right -. center))

-- | Simplify a path by dropping points too close to their neighbours. The larger
-- the cutoff parameter, the simpler the result will be.
--
-- <<docs/interpolation/3_simplify_path_radial.svg>>
simplifyTrajectoryRadial
    :: Sequential vector
    => Double      -- ^ Cutoff parameter. We remove points that are closer than this to a neighbour.
    -> vector Vec2 -- ^ Trajectory
    -> [Vec2]      -- ^ Simplified trajectory
simplifyTrajectoryRadial cutoff = simplifyTrajectoryRadialBy cutoff id . toVector

-- | 'simplifyTrajectoryRadial', but allows specifying a function for how to extract the points
-- to base simplifying on from the input.
simplifyTrajectoryRadialBy
    :: Sequential vector
    => Double      -- ^ Cutoff parameter. We remove points that are closer than this to a neighbour.
    -> (a -> Vec2) -- ^ Extract the relevant 'Vec2' to simplify on
    -> vector a    -- ^ Trajectory
    -> [a]         -- ^ Simplified trajectory
simplifyTrajectoryRadialBy cutoff vec2in = go . toVector
  where
    tooClose pivot candidate = normSquare (vec2in pivot -. vec2in candidate) <= cutoff^2
    go trajectory = case V.uncons trajectory of
        Nothing -> []
        Just (pivot,xs)
            | V.null rest && V.null toDrop -> [pivot]
            | V.null rest -> [pivot, V.last toDrop]
            | otherwise -> pivot : go rest
            where (toDrop, rest) = V.span (tooClose pivot) xs

-- | Contains at most two neighbours of a point. Can be seen as representing the
-- locations we can travel to on a line – one neighbour this way, or one neighbour
-- the other way.
--
-- It is undirected in the sense that we don’t know where the neighbours are (left
-- or right), only that they’re there.
data UndirectedNeighbourMap a = UndirectedNeighbourMap !(Map a (OneTwo a))
    deriving (Eq, Ord, Show)

data OneTwo a = One !a | Two !a !a
    deriving (Eq, Ord, Show)

-- | This instance crashes when
instance Ord a => Semigroup (UndirectedNeighbourMap a) where
    UndirectedNeighbourMap x <> UndirectedNeighbourMap y = UndirectedNeighbourMap $
        M.unionWith
            (\a b -> case (a,b) of
                (One a', One b') -> Two a' b'
                _otherwise -> error
                    "Insertion of more than two neighbours in a UndirectedNeighbourMap.\
                    \ Are you sure all your points have at most two neighbours?"
            )
            x
            y

instance Ord a => Monoid (UndirectedNeighbourMap a) where
    mempty = UndirectedNeighbourMap mempty

-- | Yield of the points the neighbour map points to, and take it out of the map
-- so we don’t follow this direction again.
--
-- Unlike when we walk in the backwards direction, we can simply pick an arbitrary
-- target to go to if we have two neighbours.
lnmLookupDeleteForward :: Ord a => a -> UndirectedNeighbourMap a -> Maybe (a, UndirectedNeighbourMap a)
lnmLookupDeleteForward x (UndirectedNeighbourMap m) = case M.lookup x m of
    Nothing        -> Nothing
    Just (One a)   -> Just (a, UndirectedNeighbourMap (M.delete x m))
    Just (Two a b) -> Just (a, UndirectedNeighbourMap (M.insert x (One b) m))

-- | Like 'lnmLookupDeleteForward', but when deleting the backwards direction, we
-- have to make sure we don’t arbitrarily delete a point we might have come from,
-- but the very point we did.
lnmLookupDeleteBackward :: Ord a => a -> a -> UndirectedNeighbourMap a -> Maybe (a, UndirectedNeighbourMap a)
lnmLookupDeleteBackward from x (UndirectedNeighbourMap m) = case M.lookup x m of
    Nothing        -> Nothing
    Just (One a)   -> Just (a, UndirectedNeighbourMap (M.delete x m))
    Just (Two a b) ->
        let (extract, keep)
                | from == a = (a,b)
                | otherwise = (b,a)
        in Just (extract, UndirectedNeighbourMap (M.insert x (One keep) m))

-- | Delete what the current point points to, and Afterwards, take out the reverse
-- direction so we don’t walk in cycles.
lnmLookupDelete :: Ord a => a -> UndirectedNeighbourMap a -> Maybe (a, UndirectedNeighbourMap a)
lnmLookupDelete start lnm = case lnmLookupDeleteForward start lnm of
    Nothing -> Nothing
    Just (target, lnm') -> case lnmLookupDeleteBackward start target lnm' of
        Nothing -> Just (target, lnm')
        Just (_xPointedToByNeighbour, lnm'') -> Just (target, lnm'')

-- lnmArbitraryElement :: UndirectedNeighbourMap a -> Maybe a
lnmArbitraryElement :: UndirectedNeighbourMap a -> Maybe a
lnmArbitraryElement (UndirectedNeighbourMap m) = fmap fst (M.lookupMin m)

-- | Given a collection of lines that fit together back-to-back (as in '=='),
-- reassemble them to extract the underlying points in order. This works even for
-- collections of multiple cut-up trajectories, as long as they do not share
-- any points.
--
-- In a way, this can be seen as the inverse of pairing up points to line segments,
--
-- @
-- 'reassembleLines' ('zipWith' 'Line' xs ('tail' xs)) '==' xs
-- @
--
-- __Unsafety warning:__ This algorithm wasn’t tested for cases when multiple
-- trajectories share points. I have no idea what happens in that case, but
-- certainly nothing useful.
reassembleLines
    :: (Ord point, Foldable f)
    => (line -> (point, point)) -- ^ How to extract two neighbouring points from the data given
    -> f line                   -- ^ Collection of neighbouring points
    -> [[point]]                -- ^ List of trajectories consisting of points
reassembleLines neighbour = fmap toList . extractAllTrajectories . buildUndirectedNeighbourMap neighbour

-- | Split a collection of line segments into a Map of ascending and descending
-- values. This is the basis for later reconnecting the line segments to extract
-- trajectories.
buildUndirectedNeighbourMap :: (Foldable f, Ord point) => (line -> (point, point)) -> f line -> UndirectedNeighbourMap point
buildUndirectedNeighbourMap neighbours = foldMap $ \line ->
    let (a,b) = neighbours line
    in UndirectedNeighbourMap (M.fromList [(a, (One b)), (b, One a)])

-- | Follow the entries in a neighbour map, starting at a point.
-- Doing this twice will grow the trajectory in both directions.
extractSingleTrajectoryPass
    :: Ord a
    => UndirectedNeighbourMap a
    -> a
    -> Seq a
    -> (UndirectedNeighbourMap a, Seq a)
extractSingleTrajectoryPass lnm start result
    | Just (next, lnm') <- lnmLookupDelete start lnm = extractSingleTrajectoryPass lnm' next (result |> next)
    | otherwise = (lnm, result)

-- | Follow the entries in a neighbour map from a starting point twice, to extend
-- it backwards and forwards as much as possible.
extractSingleTrajectory
    :: Ord a
    => UndirectedNeighbourMap a
    -> a
    -> (UndirectedNeighbourMap a, Seq a)
extractSingleTrajectory nMap start =
    let (nMapA, trajectoryPass1) = extractSingleTrajectoryPass nMap start mempty
        (nMapB, trajectoryPass2) = extractSingleTrajectoryPass nMapA start mempty
    in (nMapB, Seq.reverse trajectoryPass2 <> Seq.singleton start <> trajectoryPass1)

-- | Repeatedly extract a trajectory, until the neighbour map is exhausted.
extractAllTrajectories :: Ord a => UndirectedNeighbourMap a -> [Seq a]
extractAllTrajectories lnm
    | Just start <- lnmArbitraryElement lnm =
        let (lnm', trajectory) = extractSingleTrajectory lnm start
        in trajectory : extractAllTrajectories lnm'
    | otherwise = []
