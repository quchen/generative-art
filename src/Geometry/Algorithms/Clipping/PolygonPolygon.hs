-- | __INTERNAL MODULE__, not exposed from the package.
module Geometry.Algorithms.Clipping.PolygonPolygon (
      intersectionPP
    , differencePP
    , unionPP
) where



import           Control.Monad.Trans.State
import           Data.Coerce
import           Data.List
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S

import Geometry.Core
import Util



newtype EdgeGraph = EdgeGraph (Map Vec2 PointType)
    deriving (Eq, Ord, Show)

newtype UnvisitedEnters = UnvisitedEnters (Set Vec2)
    deriving (Eq, Ord, Show)

data CutLine ann
    = NoCut Vec2 Vec2
    | Cut ann Vec2 Vec2 Vec2
    deriving (Eq, Ord, Show)

orientPolygon :: PolygonOrientation -> Polygon -> Polygon
orientPolygon desiredOrientation polygon
    | polygonOrientation polygon == desiredOrientation = polygon
    | otherwise = reversePolygon polygon

reverseOrientation :: PolygonOrientation -> PolygonOrientation
reverseOrientation PolygonPositive = PolygonNegative
reverseOrientation PolygonNegative = PolygonPositive

reversePolygon :: Polygon -> Polygon
reversePolygon (Polygon ps) = Polygon (reverse ps)

cutPolygon :: Polygon -> Polygon -> [CutLine ()]
cutPolygon subject knives = do
    edge@(Line start end) <- polygonEdges subject
    case sortOn (\x -> positionAlongEdge x edge) (multiCutLine edge (polygonEdges knives)) of
        [] -> [NoCut start end]
        cuts -> let points = (start : cuts ++ [end])
                in zipWith3 (\a b c -> Cut () a b c) points (drop 1 points) (drop 2 points)

-- | Cut a line with multiple knives, and report the intersection points in order
-- along the edge.
multiCutLine :: Line -> [Line] -> [Vec2]
multiCutLine edge knives = [x | IntersectionReal x <- map (intersectionLL edge) knives]

-- Position of a point on a line relative to the line’s start in arbitrary units.
-- Useful for sorting.
positionAlongEdge :: Vec2 -> Line -> Double
positionAlongEdge p edge@(Line edgeStart _) = dotProduct (vectorOf edge) (vectorOf (Line edgeStart p))

-- | Annotate a line with cuts on whether the cut transitions in or out of another object.
annotateTransitions :: [CutLine a] -> Polygon -> [CutLine Transition]
annotateTransitions vertices other = go initialLocation vertices
  where
    initialLocation = case head vertices of -- head is safe here because we only force this if xs is nonempty in go
        NoCut start _ | pointInPolygon start other -> Inside
        Cut _ start _ _ | pointInPolygon start other -> Inside
        _other -> Outside

    go _ [] = []
    go inOrOut (NoCut start end : xs) = NoCut start end : go inOrOut xs
    go Inside (Cut _ start x end : xs) = Cut Exit start x end : go Outside xs
    go Outside (Cut _ start x end : xs) = Cut Enter start x end : go Inside xs

buildGraph :: [CutLine Transition] -> (EdgeGraph, UnvisitedEnters)
buildGraph vertices =
    let (graph, enterPoints) = go mempty mempty vertices
    in (EdgeGraph graph, UnvisitedEnters enterPoints)
  where
    go graph enterPoints = \case
        []                         -> (graph, enterPoints)
        NoCut     start end   : xs -> go (M.insert start (PRemain end) graph)                                    enterPoints              xs
        Cut Enter start x end : xs -> go (M.insert start (PTransition Enter x) (M.insert x (PRemain end) graph)) (S.insert x enterPoints) xs
        Cut Exit  start x end : xs -> go (M.insert start (PTransition Exit x) (M.insert x (PRemain end) graph))  enterPoints xs

data Transition = Enter | Exit deriving (Eq, Ord, Show)
data PointLocation = Inside | Outside deriving (Eq, Ord, Show)
data PointType = PRemain Vec2 | PTransition Transition Vec2 deriving (Eq, Ord, Show)

jumpToUnvisitedEnter :: State ReconstructionState (Maybe Vec2)
jumpToUnvisitedEnter = do
    UnvisitedEnters unvisitedEnters <- gets _unvisitedEnteringPoints
    case S.minView unvisitedEnters of
        Nothing -> pure Nothing
        Just (enter, rest) -> do
            modify' (\s -> s
                { _unvisitedEnteringPoints = UnvisitedEnters rest
                , _graph1Active = True
                , _currentPos = enter })
            pure (Just enter)

swapActiveGraph :: State ReconstructionState ()
swapActiveGraph = modify' (\s -> s { _graph1Active = not (_graph1Active s) })

getActiveGraph :: State ReconstructionState EdgeGraph
getActiveGraph = do
    graph1Active <- gets _graph1Active
    if graph1Active
        then gets _graph1
        else gets _graph2

markAsVisited :: Vec2 -> State ReconstructionState ()
markAsVisited x = modify' (\s -> s { _unvisitedEnteringPoints = coerced (S.delete x) (_unvisitedEnteringPoints s) })

coerced :: (Coercible a b, Coercible b a) => (a -> a) -> b -> b
coerced f = coerce . f . coerce

setCurrentPos :: Vec2 -> State ReconstructionState ()
setCurrentPos x = modify' (\s -> s { _currentPos = x })

followUntil :: Transition -> State ReconstructionState [Vec2]
followUntil transition = do
    pos <- gets _currentPos
    EdgeGraph eg <- getActiveGraph
    case M.lookup pos eg of
        Nothing -> bugError "followUntil" ("Non-looping graph: " ++ show pos ++ " has no target")
        Just (PTransition t p)
            | t /= transition -> bugError "followUntil" ("Found " ++ show t ++ " node searching for an " ++ show transition ++ "")
            | otherwise -> do
                markAsVisited p
                setCurrentPos p
                pure [p]
        Just (PRemain target) -> do
            setCurrentPos target
            rest <- followUntil transition
            pure (target : rest)

data ReconstructionState = ReconstructionState
    { _currentPos :: Vec2 -- ^ Used to check when we’re back at the start when walking along the graph
    , _unvisitedEnteringPoints :: UnvisitedEnters -- ^ Used as a list of future points to start walking on
    , _graph1Active :: Bool -- ^ Are we using graph1 (or graph2)?
    , _graph1 :: EdgeGraph -- ^ Graph of the first cut polygon
    , _graph2 :: EdgeGraph -- ^ Graph of the second cut polygon
    } deriving (Eq, Ord, Show)

-- | Given a reconstructor of one polygon, run it repeatedly to reconstruct all possible ones.
reconstructAllPolygons :: State ReconstructionState (Maybe Polygon) -> State ReconstructionState [Polygon]
reconstructAllPolygons reconstructSingle = reconstructSingle >>= \case
    Nothing -> pure []
    Just polygon -> do
        rest <- reconstructAllPolygons reconstructSingle
        pure (polygon : rest)

createReconstructionState :: Polygon -> Polygon -> ReconstructionState
createReconstructionState polygon1 polygon2 =
    let op1cut = cutPolygon polygon1 polygon2
        op2cut = cutPolygon polygon2 polygon1

        op1cutAnnotated = annotateTransitions op1cut polygon2
        op2cutAnnotated = annotateTransitions op2cut polygon1

        (op1cutGraph, op1entersOp2) = buildGraph op1cutAnnotated
        (op2cutGraph, _op2entersOp1) = buildGraph op2cutAnnotated

    in ReconstructionState
        { _currentPos = bugError "createReconstructionState" "This should be filled when needed by the algorithm, and not used otherwise!"
        , _unvisitedEnteringPoints = op1entersOp2
        , _graph1Active = bugError "createReconstructionState" "This should be filled when needed by the algorithm, and not used otherwise!"
        , _graph1 = op1cutGraph
        , _graph2 = op2cutGraph
        }

weilerAthertonIntersection :: Polygon -> Polygon -> [Polygon]
weilerAthertonIntersection polygon1 polygon2' =
    let polygon2 = orientPolygon (polygonOrientation polygon1) polygon2'

        initialState = createReconstructionState polygon1 polygon2

        reconstructIntersection = reconstructAllPolygons $ jumpToUnvisitedEnter >>= \case
            Nothing -> pure Nothing
            Just enter -> do
                let loop = do
                        -- We are at an entering point. Follow until we find an exit,
                        -- then repeat this for the other graph.
                        chain <- followUntil Exit
                        swapActiveGraph
                        currentPos <- gets _currentPos
                        if currentPos /= enter
                            then do
                                rest <- loop
                                pure (chain : rest)
                            else pure [chain]
                chains <- loop
                pure (Just (Polygon (concat chains)))
        (polygons, _finalState) = runState reconstructIntersection initialState

    in polygons

-- | All polygons resulting of the intersection of the source polygons.
intersectionPP :: Polygon -> Polygon -> [Polygon]
intersectionPP = weilerAthertonIntersection

weilerAthertonDifference :: Polygon -> Polygon -> [Polygon]
weilerAthertonDifference polygon1 polygon2' =
    let polygon2 = orientPolygon (reverseOrientation (polygonOrientation polygon1)) polygon2'

        initialState = createReconstructionState polygon1 polygon2

        reconstructDifference = reconstructAllPolygons $ jumpToUnvisitedEnter >>= \case
            Nothing -> pure Nothing
            Just start -> do
                let loop = do
                        -- We are at an entering point. Follow the other polygon until we exit again.
                        swapActiveGraph
                        chain1 <- followUntil Exit
                        -- We are now at an exit point. Follow the other (original) polygon until
                        -- we’re back at entering.
                        swapActiveGraph
                        chain2 <- followUntil Enter
                        currentPos <- gets _currentPos
                        rest <- if currentPos /= start
                            then loop
                            else pure []
                        pure (chain1 : chain2 : rest)
                chains <- loop
                pure (Just (Polygon (concat chains)))

        (polygons, _finalState) = runState reconstructDifference initialState

    in polygons

-- | All polygons resulting of the difference of the source polygons.
differencePP
    :: Polygon -- ^ A
    -> Polygon -- ^ B
    -> [Polygon] -- ^ A-B
differencePP = weilerAthertonDifference

weilerAthertonUnion :: Polygon -> Polygon -> [Polygon]
weilerAthertonUnion polygon1 polygon2' =
    let p1Orientation = polygonOrientation polygon1
        polygon2 = orientPolygon p1Orientation polygon2'

        initialState = createReconstructionState polygon1 polygon2

        reconstructUnion = reconstructAllPolygons $ jumpToUnvisitedEnter >>= \case
            Nothing -> pure Nothing
            Just start -> do
                let loop = do
                        -- We are at an entering point. Instead of entering, follow
                        -- the other polygon.
                        swapActiveGraph
                        chain <- followUntil Enter
                        currentPos <- gets _currentPos
                        rest <- if currentPos /= start
                            then loop
                            else pure []
                        pure (chain : rest)
                chains <- loop
                pure (Just (Polygon (concat chains)))

        (polygons, _finalState) = runState reconstructUnion initialState

        -- The correct result polygon has the same orientation as the original.
        -- Polygons going the wrong way round are contained within the correct
        -- union.
        removeInnerPolygons = filter (\polygon -> polygonOrientation polygon == p1Orientation)

    in removeInnerPolygons polygons

-- | All polygons resulting of the difference of the source polygons.
unionPP
    :: Polygon
    -> Polygon
    -> [Polygon]
unionPP = weilerAthertonUnion
