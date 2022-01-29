module Geometry.Trajectory (
      pointOnTrajectory
    , domain
    , simplifyTrajectory
    , reassembleLines
) where



import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Ord
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import           Data.Vector   (Vector)
import qualified Data.Vector   as V
import           Geometry.Core
import           Geometry.LUT
import           Prelude       hiding (lines)



-- | Build a lookup table from arc length to the line that we’re on at that arc
-- length.
trajectoryLut :: [Vec2] -> VLUT Double Line
trajectoryLut = VLUT . V.fromList . go 0 . pairLines
  where
    pairLines :: [Vec2] -> [Line]
    pairLines xs = zipWith Line xs (tail xs)

    go :: Double -> [Line] -> [(Double, Line)]
    go !_ [] = []
    go currentDistance (currentLine:rest)
       = let newPosition = currentDistance +. lineLength currentLine
         in (currentDistance, currentLine) : go newPosition rest

-- | Domain of distances covered by a trajectory’s LUT
domain :: VLUT Double Line -> Double
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
pointOnTrajectory :: [Vec2] -> Double -> Vec2
pointOnTrajectory points
  = let lut = trajectoryLut points
    in \dist ->
        let (start, line) = lookupBiasLower lut dist
            distLeft = dist -. start
        in moveAlongLine line distLeft


-- | Simplify a path by dropping unnecessary points. The larger the tolerance
-- distance, the simpler the result will be.
--
-- This is very useful in conjunction with 'bezierSmoothen': first drop the
-- redundancies, then smoothen using Bezier curves again, to yield a result
-- visually similar to the original data, but with a much smaller data footprint
-- (SVGs can become huge!).
--
-- This implements the Ramer-Douglas-Peucker algorithm,
-- https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
simplifyTrajectory :: Double -> [Vec2] -> [Vec2]
simplifyTrajectory epsilon  = V.toList . go . V.fromList
  where
    go :: Vector Vec2 -> Vector Vec2
    go points | V.length points <= 2 = points
    go points
      = let start = V.head points
            end = V.last points
            line = Line start end
            (dMax, iMax) = V.maximumBy (comparing fst) (V.imap (\i p -> (distanceFromLine p line, i)) points)
        in if dMax <= epsilon
            -- Points are all too close: remove them
            then V.fromList [start, end]
            -- Points far enough away: recurse
            else let before = V.take (iMax+1) points -- +1 so this includes the iMax point
                     after = V.drop iMax points
                 in go before <> V.drop 1 (go after) -- Don’t add the middle twice

-- | Given a collection of lines, put them back-to-front as much as we can, to
-- extract the underlying trajectories.
--
-- This algorithm wasn’t tested for cases when three lines originate at one point.
-- I have no idea what happens in that case, but certainly nothing useful.
reassembleLines :: Foldable f => f Line -> [Seq Vec2]
reassembleLines = extractAllTrajectories . buildNeighbourMap

-- | Split a collection of line segments into a Map of ascending and descending
-- values. This is the basis for later reconnecting the line segments to extract
-- trajectories.
buildNeighbourMap :: Foldable f => f Line -> (Map Vec2 Vec2, Map Vec2 Vec2)
buildNeighbourMap = foldMap $ \(Line a b) ->
    if a == b
        then mempty
        else (M.singleton a b, M.singleton b a)

-- | Follow the entries in a neighbour map, starting at a point.
-- Doing this twice will grow the trajectory in both directions.
extractSingleTrajectoryPass
    :: (Map Vec2 Vec2, Map Vec2 Vec2)
    -> Vec2
    -> Seq Vec2
    -> ((Map Vec2 Vec2, Map Vec2 Vec2), Seq Vec2)
extractSingleTrajectoryPass (neighboursA, neighboursB) start result
    | Just next <- M.lookup start neighboursA
        -- We follow the »start -> next« entry in A, so we delete the start in A,
        -- and so we don’t take back the same way when looking at B, we also delete
        -- the target node in B.
        --
        -- Whether we recurse on (A,B) doesn’t matter, since 'buildNeighbourMap'
        -- makes sure they don’t contain duplicates, and we look in both maps
        -- in here anyway.
        = extractSingleTrajectoryPass (M.delete start neighboursA, M.delete next neighboursB) next (result |> next)

    | Just next <- M.lookup start neighboursB
        -- Dito, but A⇆B.
        = extractSingleTrajectoryPass (M.delete next neighboursA, M.delete start neighboursB) next (result |> next)

    | otherwise = ((neighboursA, neighboursB), result)

-- | Follow the entries in a neighbour map from a starting point twice, to extend
-- it backwards and forwards as much as possible.
extractSingleTrajectory
    :: (Map Vec2 Vec2, Map Vec2 Vec2)
    -> Vec2
    -> ((Map Vec2 Vec2, Map Vec2 Vec2), Seq Vec2)
extractSingleTrajectory nMap start =
    let (nMapA, trajectoryPass1) = extractSingleTrajectoryPass nMap start mempty
        (nMapB, trajectoryPass2) = extractSingleTrajectoryPass nMapA start mempty
    in (nMapB, Seq.reverse trajectoryPass2 <> Seq.singleton start <> trajectoryPass1)

-- | Repeatedly extract a trajectory, until the neighbour map is exhausted.
extractAllTrajectories :: (Map Vec2 Vec2, Map Vec2 Vec2) -> [Seq Vec2]
extractAllTrajectories (nMapA, nMapB)
    | Just (start, nMapA') <- M.minView nMapA =
        let (nMap', trajectory) = extractSingleTrajectory (nMapA', nMapB) start
        in trajectory : extractAllTrajectories nMap'
    | Just (start, nMapB') <- M.minView nMapB =
        let (nMap', trajectory) = extractSingleTrajectory (nMapA, nMapB') start
        in trajectory : extractAllTrajectories nMap'
    | otherwise = []
