module Geometry.Trajectory.PathSimplifier.Radial where



import           Data.Sequential
import qualified Data.Vector     as V

import Geometry.Core



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
