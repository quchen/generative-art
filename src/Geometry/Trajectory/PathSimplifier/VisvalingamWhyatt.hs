-- | Thanks a lot to the Rust Geo package for this algorithm. After struggling with
-- an imperative heap-with-random-access algorithm for a while, this is a very nice
-- solution that translates somewhat well to Haskell.
--
-- Original source (MIT): https://github.com/georust/geo/blob/497d67dfa972faeac756181eb00a0c1962b0beab/geo/src/algorithm/simplifyvw.rs#L71
module Geometry.Trajectory.PathSimplifier.VisvalingamWhyatt (
      simplifyTrajectoryVW
    , simplifyTrajectoryVWBy
) where



import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable
import           Data.Heap           (Entry (..), Heap)
import qualified Data.Heap           as H
import           Data.Maybe
import           Data.Vector         (Vector, (!?))
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import Geometry.Core



data Triangle = Triangle
    { _left :: Int
    , _center :: Int
    , _right :: Int
    } deriving (Eq, Ord, Show)

triangleArea :: Vec2 -> Vec2 -> Vec2 -> Double
triangleArea left center right = abs (cross (left -. center) (right -. center))

mkTriangleAreaPQ :: Vector Vec2 -> Heap (Entry Double Triangle)
mkTriangleAreaPQ vec
    | isCyclic vec = mkTriangleAreaPQCyclic vec
    | otherwise = mkTriangleAreaPQLinear vec

mkTriangleAreaPQCyclic :: Vector Vec2 -> Heap (Entry Double Triangle)
mkTriangleAreaPQCyclic vec = heapFromVector $ V.izipWith3
    (\i a b c ->
        let l = V.length vec
        in Entry
            (triangleArea a b c)
            Triangle
                { _left   =  i    `mod` l
                , _center = (i+1) `mod` l
                , _right  = (i+2) `mod` l })
    vec
    (vec <> V.take 2 vec)
    (vec <> V.take 2 vec)

isCyclic :: Eq a => Vector a -> Bool
isCyclic vec = V.head vec == V.last vec

mkTriangleAreaPQLinear :: Vector Vec2 -> Heap (Entry Double Triangle)
mkTriangleAreaPQLinear vec = heapFromVector $ V.izipWith3
    (\i a b c -> Entry
        (triangleArea a b c)
        Triangle
            { _left   = i
            , _center = i+1
            , _right  = i+2 })
    vec
    (V.drop 1 vec)
    (V.drop 2 vec)

-- | @H.fromList . toList@ without the intermediate list.
heapFromVector :: Ord a => Vector a -> Heap a
heapFromVector = V.foldl' (\heap entry -> H.insert entry heap) mempty

-- | Yield the indices to keep from the original vector.
vwSimplifyIndices :: Double -> Vector Vec2 -> Vector Int
vwSimplifyIndices minArea inputPoints = runST $ do
    let
        inputLength = V.length inputPoints
        ix i
            | isCyclic inputPoints  = i `mod` inputLength
            | otherwise = i

    adjacentMut <- V.thaw (V.generate inputLength (\i -> (ix (i-1), ix (i+1))))
    let vwLoop heap = case H.uncons heap of
            -- Adopting a do{;} code style here, otherwise the early returns
            -- make the code run off to the right quite a bit

            Nothing -> pure ()
            Just (Entry area triangle, restOfHeap)
                -- The triangle’s area is above epsilon, skip (don’t remove) it
                | area >= minArea -> vwLoop restOfHeap
                --  This triangle’s area is below epsilon: eliminate the associated point
                | otherwise -> do

            { (left, right) <- VM.read adjacentMut (_center triangle)

            ; if left /= _left triangle || right /= _right triangle
                then
                    -- Something’s off: the actual left (right) does not match the
                    -- recorded left (right) of our triangle. The only way this can
                    -- happen is because a point in this triangle has been removed
                    -- already, and its entry is stale in the heap. (This is the
                    -- mechanism in this algorithm to remove things from deep
                    -- inside the heap: don’t actually remove them, but make it
                    -- detectable so we can do the actual removal step later, i.e.:
                    -- here.)

                    vwLoop restOfHeap
                else do

            -- We’ve got a valid triangle, and its area is smaller than epsilon, so
            -- remove it from the simulated »linked list«. As a result, we get the
            -- former left-left and right-right neighbours, which will be used to
            -- calculate the newly created triangles in the next step.
            { (ll, rr) <- deleteTriangle adjacentMut left triangle right

            -- Now recompute the adjacent triangles, using left and right adjacent
            -- points
            ; let
                newTriangles = [Triangle ll left right, Triangle left right rr]
                newHeap = insertNewTriangles inputPoints newTriangles restOfHeap

            ; vwLoop newHeap
            }}

    vwLoop (mkTriangleAreaPQ inputPoints)
    adjacent <- V.freeze adjacentMut

    -- Filter out the points that have been deleted, returning remaining point
    -- indices
    pure $ V.catMaybes $ V.izipWith
        (\i _ adj ->
            if adj /= (0,0)
                then Just i
                else Nothing)
        inputPoints
        adjacent

insertNewTriangles
    :: Vector Vec2                  -- ^ All points of the original trajectory, so we can look up coordinates by index
    -> [Triangle]                   -- ^ New triangles to insert
    -> Heap (Entry Double Triangle) -- ^ Old heap (with the center point removed)
    -> Heap (Entry Double Triangle) -- ^ New heap (with the new triangles inserted)
insertNewTriangles inputPoints current restOfHeap =
    foldl' (insertNewTriangle inputPoints) restOfHeap current

insertNewTriangle
    :: Vector Vec2                  -- ^ All points of the original trajectory, so we can look up coordinates by index
    -> Heap (Entry Double Triangle) -- ^ Old heap (with the center point removed)
    -> Triangle                     -- ^ New triangle to insert
    -> Heap (Entry Double Triangle) -- ^ New heap (with the new triangles inserted)
insertNewTriangle inputPoints heap triangle@(Triangle ai bi ci) = fromMaybe heap $ do
        guard (ai /= bi)
        guard (ai /= ci)
        guard (bi /= ci)
        a <- inputPoints !? ai
        b <- inputPoints !? bi
        c <- inputPoints !? ci
        let newArea = triangleArea a b c
        pure (H.insert (Entry newArea triangle) heap)

deleteTriangle
    :: VM.STVector s (Int, Int) -- ^ Map of index -> (index of left neighbour, index of right neighbour)
    -> Int                      -- ^ Index of new left neighbour
    -> Triangle                 -- ^ Triangle whose center is to be removed
    -> Int                      -- ^ Index of new right neighbour
    -> ST s (Int, Int)
deleteTriangle adjacentMut newLeft center newRight = do
    (ll, _l) <- VM.read adjacentMut (_left center)
    (_r, rr) <- VM.read adjacentMut (_right center)
    VM.write adjacentMut (_left center) (ll, newRight)
    VM.write adjacentMut (_right center) (newLeft, rr)
    VM.write adjacentMut (_center center) (0, 0)
    pure (ll, rr)

-- | Simplify a path by dropping unnecessary points using the the
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
simplifyTrajectoryVW epsilon trajectorySequence =
    let inputPoints = toVector trajectorySequence
        indices = vwSimplifyIndices epsilon inputPoints
    in V.backpermute inputPoints indices

-- | 'simplifyTrajectoryVWBy', but allows specifying a function for how to extract the points
-- to base simplifying on from the input.
--
-- This is useful when your trajectory contains metadata, such as the velocity at
-- each point.
simplifyTrajectoryVWBy
    :: (Ord a, Sequential vector)
    => Double      -- ^ Cutoff parameter. We remove points that span triangles smaller than this. Larger values yield simpler results.
    -> (a -> Vec2) -- ^ Extract the relevant 'Vec2' to simplify on
    -> vector a    -- ^ Trajectory
    -> Vector a    -- ^ Simplified trajectory
simplifyTrajectoryVWBy epsilon vec2in trajectorySequence =
    let inputPoints = toVector trajectorySequence
        indices = vwSimplifyIndices epsilon (V.map vec2in inputPoints)
    in V.backpermute inputPoints indices
