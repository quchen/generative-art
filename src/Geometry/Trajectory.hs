module Geometry.Trajectory (pointOnTrajectory) where

import Geometry.LUT
import Geometry.Core
import qualified Data.Vector as V

-- | Build a lookup table from arc length to the line that we’re on at that arc
-- length.
trajectoryLut :: [Vec2] -> VLUT Distance Line
trajectoryLut = VLUT . V.fromList . go (Distance 0) . pairLines
  where
    pairLines :: [Vec2] -> [Line]
    pairLines xs = zipWith Line xs (tail xs)

    go :: Distance -> [Line] -> [(Distance, Line)]
    go !_ [] = []
    go currentDistance (currentLine:rest)
       = let newPosition = currentDistance +. lineLength currentLine
         in (currentDistance, currentLine) : go newPosition rest

-- | Domain of 'Distance's covered by a trajectory’s LUT
domain :: VLUT Distance Line -> Distance
domain (VLUT lut)
  = let (allButTheLastDistance, lastLine) = V.last lut
    in allButTheLastDistance +. lineLength lastLine

-- | Walk a certain 'Distance' on a trajectory defined by its points.
--
-- This caches the internal LUT when partially applied, so that the following will
-- only compute it once for repeated lookups:
--
-- @
-- let goto = pointOnTrajectory […]
-- 'print' [goto ('Distance' d) | d <- [0, 0.1 .. 5]]
-- @
pointOnTrajectory :: [Vec2] -> Distance -> Vec2
pointOnTrajectory points
  = let lut = trajectoryLut points
    in \dist ->
        let (start, line) = lookupBiasLower lut dist
            distLeft = dist -. start
        in moveAlongLine line distLeft
