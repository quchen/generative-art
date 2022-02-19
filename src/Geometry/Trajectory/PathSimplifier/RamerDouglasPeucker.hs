module Geometry.Trajectory.PathSimplifier.RamerDouglasPeucker (
      simplifyTrajectoryRdp
    , simplifyTrajectoryRdpBy
) where



import           Data.Ord
import           Data.Sequential
import           Data.Vector     (Vector)
import qualified Data.Vector     as V

import Geometry.Core



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
-- use 'simplifyTrajectoryRdpBy'.
--
-- <<docs/interpolation/3_simplify_path_rdp.svg>>
simplifyTrajectoryRdp
    :: Sequential vector
    => Double      -- ^ Discard points closer than \(\varepsilon\) to the connecting line of two points. Larger values yield simpler results.
    -> vector Vec2 -- ^ Trajectory
    -> Vector Vec2 -- ^ Simplified trajectory
simplifyTrajectoryRdp epsilon = simplifyTrajectoryRdpBy epsilon id

-- | 'simplifyTrajectoryRdp', but allows specifying a function for how to extract the points
-- to base simplifying on from the input.
--
-- This is useful when your trajectory contains metadata, such as the velocity at
-- each point.
simplifyTrajectoryRdpBy
    :: Sequential vector
    => Double      -- ^ Discard points closer than \(\varepsilon\) to the connecting line of two points. Larger values yield simpler results.
    -> (a -> Vec2) -- ^ Extract the relevant 'Vec2' to simplify on
    -> vector a    -- ^ Trajectory
    -> Vector a    -- ^ Simplified trajectory
simplifyTrajectoryRdpBy epsilon vec2in = go . toVector
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
