{-# LANGUAGE ScopedTypeVariables #-}

module Geometry.Trajectory (
    -- * Various
      pointOnTrajectory
    , Reassemble.reassembleLines

    -- * Path simplifiers
    , simplifyTrajectory
    , simplifyTrajectoryBy

    , SimplifyRdp.simplifyTrajectoryRdp
    , SimplifyRdp.simplifyTrajectoryRdpBy

    , SimplifyVW.simplifyTrajectoryVW
    , SimplifyVW.simplifyTrajectoryVWBy

    , SimplifyRadial.simplifyTrajectoryRadial
    , SimplifyRadial.simplifyTrajectoryRadialBy
) where



import           Data.Foldable
import           Data.Sequential
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           Prelude         hiding (lines)

import Geometry.Core
import Geometry.LookupTable.Lookup1
import Geometry.Trajectory.PathSimplifier.Radial              as SimplifyRadial
import Geometry.Trajectory.PathSimplifier.RamerDouglasPeucker as SimplifyRdp
import Geometry.Trajectory.PathSimplifier.VisvalingamWhyatt   as SimplifyVW
import Geometry.Trajectory.ReassembleLines                    as Reassemble



-- | Build a lookup table from arc length to the line that we’re on at that arc
-- length.
trajectoryLut :: [Vec2] -> LookupTable1 Double Line
trajectoryLut = LookupTable1 . V.fromList . go 0 . pairLines
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

simplifyTrajectory
    :: Sequential vector
    => Double
    -> vector Vec2
    -> Vector Vec2
simplifyTrajectory = simplifyTrajectoryRdp
{-# DEPRECATED simplifyTrajectory "Old name of simplifyTrajectoryRdp" #-}

simplifyTrajectoryBy
    :: Sequential vector
    => Double
    -> (a -> Vec2)
    -> vector a
    -> Vector a
simplifyTrajectoryBy = simplifyTrajectoryRdpBy
{-# DEPRECATED simplifyTrajectoryBy "Old name of simplifyTrajectoryRdpBy" #-}
