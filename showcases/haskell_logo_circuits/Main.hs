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
import Debug.Trace


-- ghcid --command='stack ghci generative-art:exe:haskell-logo-circuits' --test=main --no-title --warnings
main :: IO ()
main =
    let picWidth = 500
        picHeight = 500
    in withSurfaceAuto "out/haskell_logo_circuits.svg" picWidth picHeight (\surface -> renderWith surface mainRender)

lambda :: Polygon
lambda = Polygon
    [ Vec2 113.386719 340.15625
    , Vec2 226.773438 170.078125
    , Vec2 113.386719 0
    , Vec2 198.425781 0
    , Vec2 425.195312 340.15625
    , Vec2 340.15625 340.15625
    , Vec2 269.292969 233.859375
    , Vec2 198.425781 340.15625
    ]

-- | Useful to limit trial and error processes: abort when health drops to zero,
-- reduce health when an impossible move was attempted.
--
-- max health (for 'heal'), current health
data Health = Health !Int !Int
    deriving (Eq, Ord, Show)

damage :: Health -> Int -> Health
damage (Health maxHealth h) dmg = Health maxHealth (h - dmg)

heal :: Health -> Health
heal (Health maxHealth h) = Health maxHealth (max maxHealth h)

isDead :: Health -> Bool
isDead (Health _ h) = h <= 0

data Circuits hex = Circuits
    { _starts :: Set hex
    , _nodes :: Map hex (CellState hex)
    } deriving (Eq, Ord, Show)

emptyCircuits :: Circuits hex
emptyCircuits = Circuits S.empty M.empty

mainRender :: Render ()
mainRender = do
    let circuits = runST $ do
            gen <- MWC.initialize (V.fromList [1252,3])
            k <- replicateM 1000 (MWC.uniformM gen)
            let _ = k :: [Int]

            let maxLength = 3
            Just x <- addCircuit gen maxLength (Health 10 10) mempty emptyCircuits
            Just y <- addCircuit gen maxLength (Health 10 10) (move R 3 mempty) x
            Just z <- addCircuit gen maxLength (Health 10 10) (move L 3 mempty) y
            let _ = x :: Circuits Cube
            pure z
    C.translate 250 250
    let cellSize = 20
    hexagonalCoordinateSystem cellSize 10
    _ <- renderCircuits cellSize circuits
    pure ()

addCircuit
    :: (HexagonalCoordinate hex, Ord hex, Show hex)
    => MWC.GenST s
    -> Int
    -> Health
    -> hex
    -> Circuits  hex
    -> ST s (Maybe (Circuits hex))
addCircuit gen maxLength health start knownCircuits = do
    dir <- randomDirection gen
    let firstStep = move dir 1 start
        fieldIsFree f = f `M.notMember` _nodes knownCircuits
    if not (fieldIsFree start) || not (fieldIsFree firstStep)
        then pure Nothing
        else do
            let acceptStep WireEnd = Just WireEnd
                acceptStep step@(WireTo target) = if fieldIsFree target then Just step else Nothing
                knownCircuitsBeforeProcess = M.insert start (WireTo firstStep) (_nodes knownCircuits)
            knownCircuitsAfterProcess <- circuitProcessFinish
                gen
                maxLength
                health
                acceptStep
                knownCircuitsBeforeProcess
                start
                firstStep
            (pure . Just) Circuits
                { _starts = S.insert start (_starts knownCircuits)
                , _nodes = knownCircuitsAfterProcess
                }

randomDirection :: MWC.GenST s -> ST s Direction
randomDirection gen = do
    n <- MWC.uniformRM (0,5) gen
    pure (V.fromList [R, UR, UL, L, DL, DR] V.! n)

data CellState hex
    = WireTo hex
    | WireEnd
    deriving (Eq, Ord, Show)

randomAction
    :: HexagonalCoordinate hex
    => MWC.GenST s
    -> hex
    -> hex
    -> ST s (CellState hex)
randomAction gen lastPos currentPos = do
    n <- MWC.uniformRM (1, 100) gen
    let _ = n :: Int
    if | n <= 50 -> pure continueStraight
       | n <= 95 -> do
           d <- MWC.uniformM gen
           pure (if d then continueRight else continueLeft)
       | otherwise -> pure terminate
  where
    straightOn = currentPos `hexAdd` hexSubtract currentPos lastPos
    right = Hex.rotateAround currentPos 1 straightOn
    left = Hex.rotateAround currentPos (-1) straightOn

    continueStraight = WireTo straightOn
    continueRight = WireTo right
    continueLeft = WireTo left
    terminate = WireEnd

circuitProcessFinish
    :: (Ord hex, HexagonalCoordinate hex)
    => MWC.Gen s
    -> Int
    -> Health
    -> (CellState hex -> Maybe (CellState hex))
    -> Map hex (CellState hex)
    -> hex
    -> hex
    -> ST s (Map hex (CellState hex))
circuitProcessFinish _gen _maxLength health _acceptStep knownCells _lastPos currentPos
    | isDead health = pure (M.insert currentPos WireEnd knownCells)
circuitProcessFinish gen maxLength health acceptStep knownCells lastPos currentPos = do
    newWire <- randomAction gen lastPos currentPos
    case acceptStep newWire of
        Nothing              -> circuitProcessFinish gen maxLength (damage health 1) acceptStep knownCells lastPos currentPos
        Just (WireTo target) -> circuitProcessFinish gen maxLength (heal health) acceptStep (M.insert currentPos newWire knownCells) currentPos target
        Just WireEnd         -> pure (M.insert currentPos newWire knownCells)

renderSingleWire
    :: (HexagonalCoordinate hex, Ord hex, Show hex)
    => Double
    -> Map hex (CellState hex)
    -> hex
    -> Render ()
renderSingleWire cellSize allKnownCells start = do
    moveToVec (toVec2 cellSize start)
    go start
  where
    go currentPosHex = case M.lookup currentPosHex allKnownCells of
        Nothing -> do
            stroke
            let !_ = trace (show currentPosHex) ()
            crossSketch (toVec2 cellSize currentPosHex) (Distance (cellSize/2))
        Just (WireTo target) -> do
            lineToVec (toVec2 cellSize target)
            go target
        Just WireEnd -> do
            stroke
            let currentPosVec2 = toVec2 cellSize currentPosHex
            cairoScope $ do
                setOperator OperatorClear
                circleSketch currentPosVec2 (Distance (cellSize/2))
                fill
            cairoScope $ do
                circleSketch currentPosVec2 (Distance (cellSize/2))
                stroke
            pure ()

renderCircuits
    :: (HexagonalCoordinate hex, Ord hex, Show hex)
    => Double
    -> Circuits hex
    -> Render ()
renderCircuits _ allCircuits
    | S.null (_starts allCircuits) = pure ()
renderCircuits cellSize allCircuits = case S.minView (_starts allCircuits) of
    Nothing -> pure ()
    Just (start, rest) -> do
        renderSingleWire cellSize (_nodes allCircuits) start
        renderCircuits cellSize allCircuits{ _starts = rest }
