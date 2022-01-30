module Geometry.Trajectory (
      pointOnTrajectory
    , domain
    , simplifyTrajectory
    , reassembleLines
) where



import           Data.Foldable
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
            isCyclic = start == end

            (dMax, iMax)
                | isCyclic = V.maximumBy (comparing fst) (V.imap (\i p -> (norm (p -. start), i)) points)
                | otherwise =
                    let connectingLine = Line start end
                    in V.maximumBy (comparing fst) (V.imap (\i p -> (distanceFromLine p connectingLine, i)) points)
        in if dMax <= epsilon
            -- Points are all too close: remove them
            then V.fromList [start, end]
            -- Points far enough away: recurse
            else let before = V.take (iMax+1) points -- +1 so this includes the iMax point
                     after = V.drop iMax points
                 in go before <> V.drop 1 (go after) -- Don’t add the middle twice

-- | Contains at most two neighbours of a point. Can be seen as representing the
-- locations we can travel to on a line – one neighbour this way, or one neighbour
-- the other way.
data LinearNeighbourMap a = LinearNeighbourMap !(Map a a) !(Map a a)
    deriving (Eq, Ord, Show)

instance Ord a => Semigroup (LinearNeighbourMap a) where
    LinearNeighbourMap a b <> LinearNeighbourMap x y = LinearNeighbourMap (a<>x) (b<>y)

instance Ord a => Monoid (LinearNeighbourMap a) where
    mempty = LinearNeighbourMap mempty mempty

-- | Given a collection of lines, put them back-to-front as much as we can, to
-- extract the underlying trajectories.
--
-- @
-- 'reassembleLines' ('zipWith' 'Line' xs ('tail' xs)) '==' xs
-- @
--
-- This algorithm wasn’t tested for cases when three lines originate at one point.
-- I have no idea what happens in that case, but certainly nothing useful.
reassembleLines
    :: (Ord point, Foldable f)
    => (line -> (point, point)) -- ^ How to extract two neighbouring points from the data given
    -> f line                   -- ^ Collection of neighbouring points
    -> [[point]]                -- ^ List of trajectories consisting of points
reassembleLines neighbour = fmap toList . extractAllTrajectories . buildLinearNeighbourMap neighbour

-- | Split a collection of line segments into a Map of ascending and descending
-- values. This is the basis for later reconnecting the line segments to extract
-- trajectories.
buildLinearNeighbourMap :: (Foldable f, Ord point) => (line -> (point, point)) -> f line -> LinearNeighbourMap point
buildLinearNeighbourMap neighbour = foldMap $ \point ->
    let (a,b) = neighbour point
    in if a == b
        then mempty
        else LinearNeighbourMap (M.singleton a b) (M.singleton b a)

-- | Follow the entries in a neighbour map, starting at a point.
-- Doing this twice will grow the trajectory in both directions.
extractSingleTrajectoryPass
    :: Ord a
    => LinearNeighbourMap a
    -> a
    -> Seq a
    -> (LinearNeighbourMap a, Seq a)
extractSingleTrajectoryPass (LinearNeighbourMap neighboursA neighboursB) start result
    | Just next <- M.lookup start neighboursA
        -- We follow the »start -> next« entry in A, so we delete the start in A,
        -- and so we don’t take back the same way when looking at B, we also delete
        -- the target node in B.
        --
        -- Whether we recurse on (A,B) doesn’t matter, since 'buildNeighbourMap'
        -- makes sure they don’t contain duplicates, and we look in both maps
        -- in here anyway.
        = extractSingleTrajectoryPass (LinearNeighbourMap  (M.delete start neighboursA) (M.delete next neighboursB)) next (result |> next)

    | Just next <- M.lookup start neighboursB
        -- Dito, but A⇆B.
        = extractSingleTrajectoryPass (LinearNeighbourMap (M.delete next neighboursA) (M.delete start neighboursB)) next (result |> next)

    | otherwise = (LinearNeighbourMap neighboursA neighboursB, result)

-- | Follow the entries in a neighbour map from a starting point twice, to extend
-- it backwards and forwards as much as possible.
extractSingleTrajectory
    :: Ord a
    => LinearNeighbourMap a
    -> a
    -> (LinearNeighbourMap a, Seq a)
extractSingleTrajectory nMap start =
    let (nMapA, trajectoryPass1) = extractSingleTrajectoryPass nMap start mempty
        (nMapB, trajectoryPass2) = extractSingleTrajectoryPass nMapA start mempty
    in (nMapB, Seq.reverse trajectoryPass2 <> Seq.singleton start <> trajectoryPass1)

-- | Repeatedly extract a trajectory, until the neighbour map is exhausted.
extractAllTrajectories :: Ord a => LinearNeighbourMap a -> [Seq a]
extractAllTrajectories (LinearNeighbourMap nMapA nMapB)
    | Just (start, nMapA') <- M.minView nMapA =
        let (nMap', trajectory) = extractSingleTrajectory (LinearNeighbourMap nMapA' nMapB) start
        in trajectory : extractAllTrajectories nMap'
    | Just (start, nMapB') <- M.minView nMapB =
        let (nMap', trajectory) = extractSingleTrajectory (LinearNeighbourMap nMapA nMapB') start
        in trajectory : extractAllTrajectories nMap'
    | otherwise = []
