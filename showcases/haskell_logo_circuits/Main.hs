module Main (main) where

import Geometry as G
import Draw as D
import Graphics.Rendering.Cairo as C hiding (x,y)
import qualified Data.Map as M
import qualified Data.Set as S
import  Data.Map (Map)
import  Data.Set (Set)
import Geometry.Coordinates.Hexagonal as Hex
import qualified System.Random.MWC as MWC
import Control.Monad.ST
import qualified Data.Vector as V
import Control.Monad
import Data.Function
import Data.Maybe
import Geometry.Chaotic
import Debug.Trace

-- ghcid --command='stack ghci generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
-- ghcid --command='stack ghci generative-art:lib generative-art:exe:haskell-logo-circuits --main-is=generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
main :: IO ()
main =
    let picWidth = 300
        picHeight = 380
    in withSurfaceAuto "out/haskell_logo_circuits.svg" picWidth picHeight (\surface -> renderWith surface mainRender)

hexLambda :: HexagonalCoordinate hex => Int -> Hex.Polygon hex
hexLambda c = Hex.Polygon $ runSteps
    [ id
    , move R (c*2)
    , move DR (c*10)
    , move L (c*2)
    , move UL (c*3)
    , move DL (c*3)
    , move L (c*2)
    , move UR (c*5)
    ]
    hexZero
  where
    runSteps [] _pos = []
    runSteps (f:fs) pos =
        let newPoint = f pos
        in newPoint : runSteps fs newPoint

randomPointInPolygon :: MWC.GenST s -> Hex.Polygon Cube -> ST s Cube
randomPointInPolygon gen poly = do
    let rMin = -10 -- This is a very poor algorithm. I apologize.
        rMax = 10  -- This is a very poor algorithm. I apologize.
        qMin = -10 -- This is a very poor algorithm. I apologize.
        qMax = 10  -- This is a very poor algorithm. I apologize.
    fix $ \loop -> do
        q <- MWC.uniformRM (qMin, qMax) gen
        r <- MWC.uniformRM (rMin, rMax) gen
        let hex = Cube q r (-q-r)
        if Hex.pointInPolygon hex poly
            then pure hex
            else loop

addCircuitInPolygon
    :: MWC.GenST s
    -> Double
    -> Hex.Polygon Cube
    -> MoveConstraints Cube
    -> Circuits Cube
    -> ST s (Circuits Cube)
addCircuitInPolygon gen cellSize poly constraints knownCircuits = fix $ \loop -> do
    pHex <- randomPointInPolygon gen poly
    if _acceptStart constraints pHex
        then addCircuit gen pHex constraints knownCircuits >>= \case
            Nothing -> loop
            Just newCircuits -> pure newCircuits
        else loop

data MoveConstraints hex = MoveConstraints
    { _acceptStart :: hex -> Bool
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

fieldIsFree :: Ord hex => hex -> Circuits hex -> Bool
fieldIsFree f circuits = f `M.notMember` _nodes circuits

circuitProcess
    :: Double
    -> Hex.Polygon Cube
    -> Circuits Cube
circuitProcess cellSize polygon = runST $ do
    gen <- MWC.initialize (V.fromList [21252,233])
    k <- replicateM 1000 (MWC.uniformM gen)
    let _ = k :: [Int]

    let acceptStep WireEnd _ = Just WireEnd
        acceptStep step@(WireTo target) knownCircuits
            | target `M.notMember` _nodes knownCircuits
                    && Hex.pointInPolygon target polygon
                = Just step
        acceptStep _ _ = Nothing

        acceptStart hex = hex `Hex.pointInPolygon` polygon && not (hex `Hex.isOnEdge` polygon)

        constraints = MoveConstraints
            { _acceptStep = acceptStep
            , _acceptStart = acceptStart
            }

    result <- iterateM 250 (addCircuitInPolygon gen cellSize polygon constraints) emptyCircuits
    pure result

mainRender :: Render ()
mainRender = do
    let cellSize = 3
        lambdaScale = 8
        lambda = hexLambda lambdaScale
    let
    C.translate 10 10
    cairoScope $ do
        setColor (mmaColor 1 1)
        Hex.polygonSketch cellSize lambda
    -- setLineWidth 1
    -- renderCircuits cellSize (circuitProcess cellSize lambda)
    -- cairoScope $ do
    --     setColor (rgba 0 0 0 0.3)
    --     polygonSketch lambda
    --     stroke


iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM n _f start | n <= 0 = pure start
iterateM n f start = f start >>= iterateM (n-1) f

addCircuit
    :: (HexagonalCoordinate hex, Ord hex)
    => MWC.GenST s
    -> hex
    -> MoveConstraints hex
    -> Circuits  hex
    -> ST s (Maybe (Circuits hex))
addCircuit gen start constraints knownCircuits = do
    dir <- randomDirection gen
    let firstStep = move dir 1 start
    if not (fieldIsFree start knownCircuits) || not (fieldIsFree firstStep knownCircuits)
        then pure Nothing
        else do
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
            pure (Just knownCircuitsAfterProcess)

randomDirection :: MWC.GenST s -> ST s Direction
randomDirection gen = do
    n <- MWC.uniformRM (0,5) gen
    pure (V.fromList [R, UR, UL, L, DL, DR] V.! n)

data CellState hex
    = WireTo hex
    | WireEnd
    deriving (Eq, Ord, Show)

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

renderSingleWire
    :: (HexagonalCoordinate hex, Ord hex)
    => Double
    -> Map hex (CellState hex)
    -> hex
    -> Render ()
renderSingleWire cellSize allKnownCells start = do
    moveToVec (toVec2 cellSize start)
    fix (\go currentPosHex -> case M.lookup currentPosHex allKnownCells of
            Nothing -> do
                stroke
                crossSketch (toVec2 cellSize currentPosHex) (Distance (cellSize/2))
            Just (WireTo target) -> do
                case M.lookup target allKnownCells of
                    Just WireEnd -> do
                        let circleRadius = cellSize/2
                            currentPosVec = toVec2 cellSize currentPosHex
                            circleCenterVec = toVec2 cellSize target
                            Line _ targetVecShortened = resizeLine (\(Distance d) -> Distance (d - circleRadius)) (Line currentPosVec circleCenterVec)
                        lineToVec targetVecShortened
                        stroke
                        circleSketch circleCenterVec (Distance circleRadius)
                        stroke
                    _other -> lineToVec (toVec2 cellSize target)
                go target
            Just WireEnd ->
                -- We handle this case in the WireTo part so we can shorten the line leading
                -- to the circle to avoid circle/line overlap
                pure ()
        )
        start

renderCircuits
    :: (HexagonalCoordinate hex, Ord hex)
    => Double
    -> Circuits hex
    -> Render ()
renderCircuits cellSize allCircuits = case S.minView (_starts allCircuits) of
    Nothing -> pure ()
    Just (start, rest) -> do
        n <- liftIO $ do
            gen <- MWC.initialize (V.fromList [fromIntegral $ perturb (S.size (_starts allCircuits))])
            MWC.uniformRM (0,2) gen
        [ darker, dark, brighter ] !! n
        renderSingleWire cellSize (_nodes allCircuits) start
        renderCircuits cellSize allCircuits{ _starts = rest }
  where
    darker = setColor (hsva 257 0.40 0.38 1)
    dark = setColor (hsva 256 0.40 0.50 1)
    brighter = setColor (hsva 304 0.45 0.56 1)
