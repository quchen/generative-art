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
reassembleLines :: Foldable f => f Line -> [Seq Vec2]
reassembleLines = extractAllTrajectories . buildNeighbourMap

-- | Split a collection of line segments into a Map of ascending and descending
-- values. This is the basis for later reconnecting the line segments to extract
-- trajectories.
buildNeighbourMap :: Foldable f => f Line -> (Map Vec2 Vec2, Map Vec2 Vec2)
buildNeighbourMap = foldMap $ \(Line start end) ->
    let a = min start end
        b = max start end
    in if start == end
        then mempty
        else (M.singleton a b, M.singleton b a)

-- | Follow the entries in a neighbour map, giving precedence to the first list.
-- This can be seen as following the instructions in the forward direction.
extractSingleTrajectoryAFirst
    :: (Map Vec2 Vec2, Map Vec2 Vec2)
    -> Vec2
    -> Seq Vec2
    -> ((Map Vec2 Vec2, Map Vec2 Vec2), Seq Vec2)
extractSingleTrajectoryAFirst (neighboursA, neighboursB) start result =
    case M.lookup start neighboursA of
        Just next -> extractSingleTrajectoryAFirst (M.delete start neighboursA, M.delete next neighboursB) next (result |> next)
        Nothing -> case M.lookup start neighboursB of
            Just next -> extractSingleTrajectoryAFirst (M.delete next neighboursA, M.delete start neighboursB) next (result |> next)
            Nothing -> ((neighboursA, neighboursB), result)

-- | Follow the entries in a neighbour map, giving precedence to the second list.
-- This can be seen as following the instructions in the backward direction.
extractSingleTrajectoryBFirst
    :: (Map Vec2 Vec2, Map Vec2 Vec2)
    -> Vec2
    -> Seq Vec2
    -> ((Map Vec2 Vec2, Map Vec2 Vec2), Seq Vec2)
extractSingleTrajectoryBFirst (neighboursA, neighboursB) = extractSingleTrajectoryAFirst (neighboursB, neighboursA)

-- | Follow the entries in a neighbour map, first forward then backward from the start, to form a single trajectory
-- extending maximally in both directions.
extractSingleTrajectory
    :: (Map Vec2 Vec2, Map Vec2 Vec2)
    -> Vec2
    -> ((Map Vec2 Vec2, Map Vec2 Vec2), Seq Vec2)
extractSingleTrajectory neighbourMap start =
    let (neighbourMapA, trajectoryA) = extractSingleTrajectoryAFirst neighbourMap start mempty
        (neighbourMapB, trajectoryB) = extractSingleTrajectoryBFirst neighbourMapA start mempty
    in (neighbourMapB, Seq.reverse trajectoryB <> trajectoryA)

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
