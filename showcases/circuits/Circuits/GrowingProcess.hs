module Circuits.GrowingProcess (
      circuitProcess
    , CellState(..)
    , Circuits(..)
    , ProcessGeometry(..)
) where



import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.ST
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as S
import qualified Data.Vector.Extended as V
import qualified System.Random.MWC    as MWC

import Geometry.Coordinates.Hexagonal as Hex



-- | The geometry in which a circuit growing process takes place.
data ProcessGeometry = ProcessGeometry
    { _inside :: Set Hex
    , _edge :: Set Hex
    } deriving (Eq, Ord, Show)

data CellState
    = WireTo Hex
    | WireEnd
    deriving (Eq, Ord, Show)

instance NFData CellState where
    rnf (WireTo target) = rnf target
    rnf WireEnd = ()

data MoveConstraints = MoveConstraints
    { _isInBounds :: Hex -> Bool
    , _acceptStep :: CellState -> Circuits -> Maybe CellState
    }

-- | All existing circuits. The wires can be reconstructed from this.
data Circuits = Circuits
    { _starts :: Set Hex
    , _nodes :: Map Hex CellState
    } deriving (Eq, Ord, Show)

instance NFData Circuits where
    rnf Circuits{_starts=starts, _nodes=nodes}
        = rnf starts `seq` rnf nodes

emptyCircuits :: Circuits
emptyCircuits = Circuits
    { _starts = S.empty
    , _nodes = M.empty
    }

insertNode :: Hex -> CellState -> Circuits -> Circuits
insertNode cellPos cellState circuits = circuits { _nodes = M.insert cellPos cellState (_nodes circuits) }

insertStart :: Hex -> Circuits -> Circuits
insertStart start circuits = circuits { _starts = S.insert start (_starts circuits) }

-- | Grow circuits inside a geometry.
circuitProcess
    :: ProcessGeometry
    -> Circuits
circuitProcess processGeometry = runST $ do
    gen <- MWC.initialize (V.fromList [252,231233,2333,233,1])
    k <- replicateM 1000 (MWC.uniformM gen) -- Warm up MWC gen
    let _ = k :: [Int]

    let acceptStep WireEnd _ = Just WireEnd
        acceptStep step@(WireTo target) knownCircuits
            | target `M.notMember` _nodes knownCircuits
              && (target `S.member` _inside processGeometry || target `S.member` _edge processGeometry)
                = Just step
        acceptStep _ _ = Nothing

        isInBounds p = p `S.member` _inside processGeometry
                    || p `S.member` _edge processGeometry

        constraints = MoveConstraints
            { _acceptStep = acceptStep
            , _isInBounds = isInBounds
            }

    (_starts, result) <- iterateUntilNothingM
        (growSingleCircuit gen constraints)
        (_inside processGeometry <> _edge processGeometry, emptyCircuits)
    pure result

iterateUntilNothingM
    :: Monad m
    => (a -> m (Maybe a))
    -> a
    -> m a
iterateUntilNothingM f = go
  where
    go x = f x >>= \case
        Nothing -> pure x
        Just x' -> go x'

growSingleCircuit
    :: MWC.Gen s
    -> MoveConstraints
    -> (Set Hex, Circuits)
    -> ST s (Maybe (Set Hex, Circuits))
growSingleCircuit gen constraints (startingCandidates, knownCircuits) =
    pickStartAndFirstStep gen constraints (startingCandidates, knownCircuits) >>= \case
        NoFirstStepPossible -> pure Nothing
        FirstStepIs thinnedOutSCs start firstStep -> do
            grownCircuit <- growCircuit gen start firstStep constraints knownCircuits
            pure (Just (thinnedOutSCs, grownCircuit))

data FirstStep
    = NoFirstStepPossible
    | FirstStepIs (Set Hex) Hex Hex
    deriving (Eq, Ord, Show)

-- | Pick a starting point and a first step. If a listed point is an impossible
-- start, remove it from the list of possible starts.
pickStartAndFirstStep
    :: MWC.GenST s
    -> MoveConstraints
    -> (Set Hex, Circuits) -- ^ Possible starting points, existing circuits
    -> ST s FirstStep
pickStartAndFirstStep gen constraints (startingCandidates, knownCircuits) =
    let allowedSCs = S.filter (\start -> fieldIsAllowed start knownCircuits constraints) startingCandidates
        loop thinnedOutSCs = randomEntry gen thinnedOutSCs >>= \case
            Nothing -> pure NoFirstStepPossible
            Just start -> randomFirstStep gen start knownCircuits constraints >>= \case
                Nothing -> loop (S.delete start thinnedOutSCs)
                Just firstStep -> pure (FirstStepIs thinnedOutSCs start firstStep)
    in loop allowedSCs

randomEntry :: MWC.GenST s -> Set a -> ST s (Maybe a)
randomEntry gen xs = do
    let n = S.size xs
    if n <= 0 then
        pure Nothing
        else do
            i <- MWC.uniformRM (0,n-1) gen
            pure (Just (S.elemAt i xs))

randomFirstStep
    :: MWC.Gen s
    -> Hex
    -> Circuits
    -> MoveConstraints
    -> ST s (Maybe Hex)
randomFirstStep gen start knownCircuits constraints = do
    let neighbours = V.fromList (ring 1 start)
    scrambledNeighbours <- do
        vMut <- V.thaw neighbours
        V.fisherYatesShuffle gen vMut
        V.unsafeFreeze vMut
    pure (V.find (\firstStep -> fieldIsAllowed firstStep knownCircuits constraints) scrambledNeighbours)

fieldIsAllowed :: Hex -> Circuits -> MoveConstraints -> Bool
fieldIsAllowed hex circuits constraints = not inAnyCircuit && inBounds
  where
    inAnyCircuit = hex `M.member` _nodes circuits
    inBounds = _isInBounds constraints hex

growCircuit
    :: MWC.Gen s
    -> Hex
    -> Hex
    -> MoveConstraints
    -> Circuits
    -> ST s Circuits
growCircuit gen start firstStep constraints knownCircuits = do
    let knownCircuitsBeforeProcess = insertStart start (insertNode start (WireTo firstStep) knownCircuits)
        loop newKnownCircuits lastPos currentPos = do
            action <- randomPossibleAction gen constraints newKnownCircuits lastPos currentPos
            case action of
                WireTo target -> loop (insertNode currentPos action newKnownCircuits) currentPos target
                WireEnd       -> pure (insertNode currentPos WireEnd newKnownCircuits)
    loop knownCircuitsBeforeProcess start firstStep

randomPossibleAction
    :: MWC.GenST s
    -> MoveConstraints
    -> Circuits
    -> Hex
    -> Hex
    -> ST s CellState
randomPossibleAction gen constraints knownCircuits lastPos currentPos = weightedRandom gen possibleActions
  where
    actions =
        [ (100, continueStraight)
        , (25, continueRight)
        , (25, continueLeft)
        , (5, terminate) -- This needs to be a valid choice as a fallback if nothing else goes
        ]

    possibleActions = flip filter actions $ \(_weight, action) ->
        isJust (_acceptStep constraints action knownCircuits)

    straightOn i = currentPos `hexAdd` hexTimes i (hexSubtract currentPos lastPos)
    right = Hex.rotateAround currentPos 1 (straightOn 1)
    left = Hex.rotateAround currentPos (-1) (straightOn 1)

    continueStraight = WireTo (straightOn 1)
    continueRight = WireTo right
    continueLeft = WireTo left
    terminate = WireEnd

-- | Pick an element from a list with a certain weight.
--
-- The probability of an entry is thus \(\frac\text{weight}\text{\sum weights}}\).
weightedRandom :: MWC.GenST s -> [(Int, a)] -> ST s a
weightedRandom _ []
    = error "weightedRandom: empty list of choices"
weightedRandom _ choices
    | any (< 0) weights = error ("weightedRandom: negative weight, " ++ show weights)
    | all (== 0) weights = error ("weightedRandom: all weights were zero, " ++ show weights)
  where
    weights = [weight | (weight, _val) <- choices]
weightedRandom gen choices = do
    let total = sum [weight | (weight, _val) <- choices]
    i <- MWC.uniformRM (1, total) gen
    pure (pick i choices)
  where
    pick n ((weight, x):xs)
        | n <= weight = x
        | otherwise   = pick (n-weight) xs
    pick _ _  = error "weightedRandom.pick used with empty list"
