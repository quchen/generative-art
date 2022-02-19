{-# LANGUAGE RecordWildCards #-}

-- | Thanks a lot to the Rust Geo package for this algorithm. After struggling with
-- an imperative heap-with-random-access algorithm for a while, this is a very nice
-- solution that translates somewhat well to Haskell.
--
-- Original source (MIT): https://github.com/georust/geo/blob/497d67dfa972faeac756181eb00a0c1962b0beab/geo/src/algorithm/simplifyvw.rs#L71
module Geometry.Trajectory.PathSimplifier.VisvalingamWhyatt (
      simplifyTrajectoryVW
    , simplifyTrajectoryVWBy
) where



import           Control.Monad.ST
import           Data.Foldable
import           Data.Heap           (Entry (..), Heap)
import qualified Data.Heap           as H
import           Data.Maybe
import           Data.Vector         (Vector, (!?))
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

import Geometry.Core



data LeftCurrentRight = LeftCurrentRight
    { _current :: Int
    , _left :: Int
    , _right :: Int
    } deriving (Eq, Ord, Show)

triangleArea :: Vec2 -> Vec2 -> Vec2 -> Double
triangleArea left center right = abs (det (left -. center) (right -. center))

mkTriangleAreaPQ :: Vector Vec2 -> Heap (Entry Double LeftCurrentRight)
mkTriangleAreaPQ vec = H.fromList . toList $ V.izipWith3
    (\i a b c -> Entry
        (triangleArea a b c)
        LeftCurrentRight { _left=i, _current=i+1, _right=i+2 })
    vec
    (V.drop 1 vec)
    (V.drop 2 vec)

-- | Yield the indices to keep from the original vector.
vwSimplifyIndices :: Double -> Vector Vec2 -> Vector Int
vwSimplifyIndices minArea inputPoints = runST $ do
    adjacentMut <- V.thaw (V.generate (V.length inputPoints) (\i -> (i-1, i+1)))
    let vwLoop heap = case H.uncons heap of
            Nothing -> pure ()
            Just (Entry area smallestTriangle, restOfHeap)
                -- The triangle’s area is above epsilon, skip (don’t remove) it
                | area >= minArea -> vwLoop restOfHeap
                --  This triangle’s area is below epsilon: eliminate the associated point
                | otherwise -> do
                    let _ = heap :: Heap (Entry Double LeftCurrentRight)
                    (left, right) <- VM.read adjacentMut (_current smallestTriangle)
                    if left /= _left smallestTriangle || right /= _right smallestTriangle
                        then
                            -- A point in this triangle has been removed since the
                            -- Entry was created, skip it
                            vwLoop restOfHeap
                        else do
                            -- We’ve got a valid triangle, and its area is smaller
                            -- than epsilon, so remove it from the simulated
                            -- »linked list«
                            (ll, _) <- VM.read adjacentMut (_left smallestTriangle)
                            (_, rr) <- VM.read adjacentMut (_right smallestTriangle)
                            VM.write adjacentMut (_left smallestTriangle) (ll, right)
                            VM.write adjacentMut (_right smallestTriangle) (left, rr)
                            VM.write adjacentMut (_current smallestTriangle) (0, 0)

                            -- Now recompute the adjacent triangles, using
                            -- left and right adjacent points
                            let choices = [(ll, left, right), (left, right, rr)]
                                newHeap = foldl'
                                    (\acc (ai, bi, ci) -> fromMaybe acc $ do
                                        a <- inputPoints !? ai
                                        b <- inputPoints !? bi
                                        c <- inputPoints !? ci
                                        let newArea = triangleArea a b c
                                            lcr = LeftCurrentRight
                                                        { _left    = ai
                                                        , _current = bi
                                                        , _right   = ci}
                                        pure (H.insert (Entry newArea lcr) acc))
                                    restOfHeap
                                    choices

                            vwLoop newHeap

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
