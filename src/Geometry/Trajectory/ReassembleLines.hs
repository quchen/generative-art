module Geometry.Trajectory.ReassembleLines (reassembleLines) where



import           Data.Foldable
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Ord
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import           Prelude       hiding (lines)



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
