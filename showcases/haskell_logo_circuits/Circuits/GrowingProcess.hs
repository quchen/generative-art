module Circuits.GrowingProcess (
    circuitProcess
    , CellState(..)
    , Circuits(..)
    , ProcessGeometry(..)
) where



import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import qualified Data.Vector                    as V
import           Draw                           as D
import           Geometry.Coordinates.Hexagonal as Hex
import qualified System.Random.MWC              as MWC
import           Why                            (fisherYatesShuffle)



data ProcessGeometry hex = ProcessGeometry
    { _inside :: Set hex
    , _edge :: Set hex
    } deriving (Eq, Ord, Show)

data CellState hex
    = WireTo hex
    | WireEnd
    deriving (Eq, Ord, Show)

data MoveConstraints hex = MoveConstraints
    { _isInBounds :: hex -> Bool
    , _acceptStep :: CellState hex -> Circuits hex -> Maybe (CellState hex)
    }

data Circuits hex = Circuits
    { _starts :: Set hex
    , _nodes :: Map hex (CellState hex)
    } deriving (Eq, Ord, Show)

emptyCircuits :: Circuits hex
emptyCircuits = Circuits
    { _starts = S.empty
    , _nodes = M.empty
    }

insertNode :: Ord hex => hex -> CellState hex -> Circuits hex -> Circuits hex
insertNode cellPos cellState circuits = circuits { _nodes = M.insert cellPos cellState (_nodes circuits) }

insertStart :: Ord hex => hex -> Circuits hex -> Circuits hex
insertStart start circuits = circuits { _starts = S.insert start (_starts circuits) }

circuitProcess
    :: ProcessGeometry Cube
    -> Circuits Cube
circuitProcess processGeometry = runST $ do
    gen <- MWC.initialize (V.fromList [252,231233,2333,233])
    k <- replicateM 1000 (MWC.uniformM gen)
    let _ = k :: [Int]

    let acceptStep WireEnd _ = Just WireEnd
        acceptStep step@(WireTo target) knownCircuits
            | target `M.notMember` _nodes knownCircuits
                    && target `S.member` (_inside processGeometry <> _edge processGeometry)
                = Just step
        acceptStep _ _ = Nothing

        isInBounds p = p `S.member` _inside processGeometry
                    || p `S.member` _edge processGeometry

        constraints = MoveConstraints
            { _acceptStep = acceptStep
            , _isInBounds = isInBounds
            }

    (_starts, result) <- iterateUntilM
        (\(startingCandidates, _) -> S.null startingCandidates)
        (growSingleCircuit gen constraints)
        (_inside processGeometry, emptyCircuits)
    pure result

iterateUntilM :: Monad f => (a -> Bool) -> (a -> f a) -> a -> f a
iterateUntilM p = go
  where
    go _f x | p x = pure x
    go f x = f x >>= go f

growSingleCircuit
    :: MWC.Gen s
    -> MoveConstraints Cube
    -> (Set Cube, Circuits Cube)
    -> ST s (Set Cube, Circuits Cube)
growSingleCircuit gen constraints (initialStartingCandidates, knownCircuits) =
    fix
        (\loop startingCandidates -> do
            startCandidate <- randomEntry gen startingCandidates
            case startCandidate of
                Nothing -> pure (mempty, knownCircuits)
                Just start -> do
                    stepCandidate <- randomFirstStep gen start knownCircuits constraints
                    case stepCandidate of
                        Nothing -> loop (S.delete start startingCandidates)
                        Just firstStep -> do
                            grownCircuit <- growCircuit gen start firstStep constraints knownCircuits
                            let startCandidates' = startingCandidates `S.difference` M.keysSet (_nodes knownCircuits)
                            pure (startCandidates', grownCircuit)
        )
        initialStartingCandidates

randomEntry :: Foldable f => MWC.GenST s -> f a -> ST s (Maybe a)
randomEntry gen xs = do
    let n = length xs
    if n <= 0 then
        pure Nothing
        else do
            i <- MWC.uniformRM (0,n-1) gen
            pure (Just (toList xs !! i))

randomFirstStep
    :: (HexagonalCoordinate hex, Ord hex)
    => MWC.Gen s
    -> hex
    -> Circuits hex
    -> MoveConstraints hex
    -> ST s (Maybe hex)
randomFirstStep gen start knownCircuits constraints = do
    let neighbours = V.fromList (ring 1 start)
    scrambledNeighbours <- fisherYatesShuffle gen neighbours
    pure (V.find (\firstStep -> fieldIsAllowed firstStep knownCircuits constraints) scrambledNeighbours)

fieldIsAllowed :: Ord hex => hex -> Circuits hex -> MoveConstraints hex -> Bool
fieldIsAllowed hex circuits constraints = not inAnyCircuit && inBounds
  where
    inAnyCircuit = hex `M.member` _nodes circuits
    inBounds = _isInBounds constraints hex

growCircuit
    :: (HexagonalCoordinate hex, Ord hex)
    => MWC.Gen s
    -> hex
    -> hex
    -> MoveConstraints hex
    -> Circuits hex
    -> ST s (Circuits hex)
growCircuit gen start firstStep constraints knownCircuits = do
    let knownCircuitsBeforeProcess = insertStart start (insertNode start (WireTo firstStep) knownCircuits)
    knownCircuitsAfterProcess <- fix
        (\loop newKnownCircuits lastPos currentPos -> do
            action <- randomPossibleAction gen constraints newKnownCircuits lastPos currentPos
            case action of
                WireTo target -> loop (insertNode currentPos action newKnownCircuits) currentPos target
                WireEnd       -> pure (insertNode currentPos WireEnd newKnownCircuits)
        )
        knownCircuitsBeforeProcess
        start
        firstStep
    pure knownCircuitsAfterProcess

randomPossibleAction
    :: HexagonalCoordinate hex
    => MWC.GenST s
    -> MoveConstraints hex
    -> Circuits hex
    -> hex
    -> hex
    -> ST s (CellState hex)
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
