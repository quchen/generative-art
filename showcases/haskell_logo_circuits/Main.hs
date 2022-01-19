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
lambda = G.transform (G.translate (negateV bbRawPoly)) rawPoly
  where
    bbRawPoly = boundingBoxCenter rawPoly
    rawPoly = Polygon
        [ Vec2 113.386719 340.15625
        , Vec2 226.773438 170.078125
        , Vec2 113.386719 0
        , Vec2 198.425781 0
        , Vec2 425.195312 340.15625
        , Vec2 340.15625 340.15625
        , Vec2 269.292969 233.859375
        , Vec2 198.425781 340.15625
        ]

randomPointInPolygon :: MWC.GenST s -> Polygon -> ST s Vec2
randomPointInPolygon gen poly = do
    let (BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax)) = boundingBox poly
    fix $ \loop -> do
        x <- MWC.uniformRM (xMin, xMax) gen
        y <- MWC.uniformRM (yMin, yMax) gen
        let v = Vec2 x y
        if pointInPolygon v poly
            then pure v
            else loop

addCircuitInPolygon gen cellSize poly acceptStep knownCircuits = do
    fix $ \loop -> do
        p <- randomPointInPolygon gen poly
        let pHex = fromVec2 cellSize p
            maxLength = error "silly parameter remove me"
        result <- addCircuit gen maxLength (Health 10 10) pHex acceptStep knownCircuits
        case result of
            Nothing -> loop
            Just newCircuits -> pure newCircuits

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
    let cellSize = 5
    let circuits = runST $ do
            gen <- MWC.initialize (V.fromList [21252,233])
            k <- replicateM 1000 (MWC.uniformM gen)
            let _ = k :: [Int]

            let acceptStep WireEnd _ = Just WireEnd
                acceptStep step@(WireTo target) knownCircuits
                    | target `M.notMember` _nodes knownCircuits
                         && pointInPolygon (toVec2 cellSize target) lambda
                        = Just step
                acceptStep _ _ = Nothing

            let iterateM n f start
                    | n == 0 = pure start
                    | otherwise = f start >>= iterateM (n-1) f
            result <- iterateM 150 (addCircuitInPolygon gen cellSize lambda acceptStep) emptyCircuits
            let _ = result :: Circuits Cube
            pure result
    C.translate 250 250
    -- cairoScope $ do
    --     polygonSketch lambda
    --     stroke
    -- hexagonalCoordinateSystem cellSize 10
    _ <- renderCircuits cellSize circuits
    pure ()

addCircuit
    :: (HexagonalCoordinate hex, Ord hex, Show hex)
    => MWC.GenST s
    -> Int
    -> Health
    -> hex
    -> (CellState hex -> Circuits hex -> Maybe (CellState hex))
    -> Circuits  hex
    -> ST s (Maybe (Circuits hex))
addCircuit gen maxLength health start acceptStep knownCircuits = do
    dir <- randomDirection gen
    let firstStep = move dir 1 start
        fieldIsFree f = f `M.notMember` _nodes knownCircuits
    if not (fieldIsFree start) || not (fieldIsFree firstStep)
        then pure Nothing
        else do
            let knownCircuitsBeforeProcess = knownCircuits
                    { _starts = S.insert start (_starts knownCircuits)
                    , _nodes = M.insert start (WireTo firstStep) (_nodes knownCircuits)
                    }
            knownCircuitsAfterProcess <- circuitProcessFinish
                gen
                maxLength
                health
                acceptStep
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
    -> (CellState hex -> Circuits hex -> Maybe (CellState hex))
    -> Circuits hex
    -> hex
    -> hex
    -> ST s (Circuits hex)
circuitProcessFinish _gen _maxLength health _acceptStep knownCircuits _lastPos currentPos
    | isDead health = pure knownCircuits { _nodes = M.insert currentPos WireEnd (_nodes knownCircuits) }
circuitProcessFinish gen maxLength health acceptStep knownCircuits lastPos currentPos = do
    newWire <- randomAction gen lastPos currentPos
    case acceptStep newWire knownCircuits of
        Nothing              -> circuitProcessFinish gen maxLength (damage health 1) acceptStep knownCircuits lastPos currentPos
        Just (WireTo target) -> circuitProcessFinish gen maxLength (heal health) acceptStep knownCircuits{ _nodes = M.insert currentPos newWire (_nodes knownCircuits) } currentPos target
        Just WireEnd         -> pure knownCircuits { _nodes = M.insert currentPos WireEnd (_nodes knownCircuits) }

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
