-- | __INTERNAL MODULE__, not exposed from the package.
module Geometry.Algorithms.Cut.WeilerAtherton (intersectionOfTwoPolygons) where



import           Control.Monad.Trans.State
import           Data.List
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S

import Geometry.Core
import Util



newtype EdgeGraph = EdgeGraph (Map Vec2 PointType)
    deriving (Eq, Ord, Show)

data CutLine ann
    = NoCut Vec2 Vec2
    | Cut ann Vec2 Vec2 Vec2
    deriving (Eq, Ord, Show)

-- | Newtype tag to signify the contained polygon is oriented.
newtype Oriented a = Oriented a
    deriving (Eq, Ord, Show)

orientPolygon :: PolygonOrientation -> Polygon -> Oriented Polygon
orientPolygon desiredOrientation polygon = if polygonOrientation polygon == desiredOrientation
    then Oriented polygon
    else let Polygon ps = polygon in Oriented (Polygon (reverse ps))

cutPolygon :: Oriented Polygon -> Polygon -> Oriented [CutLine ()]
cutPolygon (Oriented subject) knives = Oriented $ do
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
annotateTransitions :: Oriented [CutLine a] -> Polygon -> Oriented [CutLine Transition]
annotateTransitions (Oriented vertices) other = Oriented (go initialLocation vertices)
  where
    initialLocation = case head vertices of -- head is safe here because we only force this if xs is nonempty in go
        NoCut start _ | pointInPolygon start other -> Inside
        Cut _ start _ _ | pointInPolygon start other -> Inside
        _other -> Outside

    go _ [] = []
    go inOrOut (NoCut start end : xs) = NoCut start end : go inOrOut xs
    go Inside (Cut _ start x end : xs) = Cut Exit start x end : go Outside xs
    go Outside (Cut _ start x end : xs) = Cut Enter start x end : go Inside xs

buildGraph :: Oriented [CutLine Transition] -> (EdgeGraph, Set Vec2)
buildGraph (Oriented vertices) =
    let (graph, enterPoints) = go mempty mempty vertices
    in (EdgeGraph graph, enterPoints)
  where
    go graph enterPoints = \case
        []                         -> (graph, enterPoints)
        NoCut     start end   : xs -> go (M.insert start (PRemain end) graph)                                    enterPoints              xs
        Cut Enter start x end : xs -> go (M.insert start (PTransition Enter x) (M.insert x (PRemain end) graph)) (S.insert x enterPoints) xs
        Cut Exit  start x end : xs -> go (M.insert start (PTransition Exit x) (M.insert x (PRemain end) graph))  enterPoints xs

data Transition = Enter | Exit deriving (Eq, Ord, Show)
data PointLocation = Inside | Outside deriving (Eq, Ord, Show)
data PointType = PRemain Vec2 | PTransition Transition Vec2 deriving (Eq, Ord, Show)

-- | Walk a single cycle to reconstruct one polygon.
reconstructSingleUnionPolygon :: State ReconstructionState (Maybe Polygon)
reconstructSingleUnionPolygon = jumpToUnvisitedEnter >>= \case
    Nothing -> pure Nothing
    Just enter -> do
        modify' (\s -> s { _graph1Active = True })
        let loop = do
                chain <- followUntilExitS
                currentPos <- gets _currentPos
                if currentPos /= enter
                    then do
                        swapActiveGraph
                        rest <- loop
                        pure (chain : rest)
                    else pure [chain]
        chains <- loop
        pure (Just (Polygon (concat chains)))

jumpToUnvisitedEnter :: State ReconstructionState (Maybe Vec2)
jumpToUnvisitedEnter = do
    unvisitedEnters <- gets (S.minView . _unvisitedEnteringPoints)
    case unvisitedEnters of
        Nothing -> pure Nothing
        Just (enter, rest) -> do
            modify' (\s -> s
                { _unvisitedEnteringPoints = rest
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
markAsVisited x = modify' (\s -> s { _unvisitedEnteringPoints = S.delete x (_unvisitedEnteringPoints s) })

setCurrentPos :: Vec2 -> State ReconstructionState ()
setCurrentPos x = modify' (\s -> s { _currentPos = x })

followUntilExitS :: State ReconstructionState [Vec2]
followUntilExitS = do
    pos <- gets _currentPos
    EdgeGraph eg <- getActiveGraph
    case M.lookup pos eg of
        Nothing -> error ("Bad edge graph: " ++ show pos ++ " has no target")
        Just (PTransition Enter _) -> error "Found enter node searching for an exit"
        Just (PTransition Exit exit) -> do
            markAsVisited exit
            setCurrentPos exit
            pure [exit]
        Just (PRemain target) -> do
            setCurrentPos target
            rest <- followUntilExitS
            pure (target : rest)

data ReconstructionState = ReconstructionState
    { _currentPos :: Vec2 -- ^ Used to check when we’re back at the start when walking along the graph
    , _unvisitedEnteringPoints :: Set Vec2 -- ^ Used as a list of future points to start walking on
    , _graph1Active :: Bool -- ^ Are we using graph1 (or graph2)?
    , _graph1 :: EdgeGraph -- ^ Graph of the first cut polygon
    , _graph2 :: EdgeGraph -- ^ Graph of the second cut polygon
    } deriving (Eq, Ord, Show)

-- | Reconstruct all polygons until the input is exhausted.
reconstructAllUnionPolygons :: State ReconstructionState [Polygon]
reconstructAllUnionPolygons = reconstructSingleUnionPolygon >>= \case
    Nothing -> pure []
    Just polygon -> do
        rest <- reconstructAllUnionPolygons
        pure (polygon : rest)

weilerAthertonUnion :: Polygon -> Polygon -> [Polygon]
weilerAthertonUnion polygon1 polygon2 =
    let op1 = orientPolygon PolygonPositive polygon1
        op2 = orientPolygon PolygonPositive polygon2

        op1cut = cutPolygon op1 polygon2
        op2cut = cutPolygon op2 polygon1

        op1cutAnnotated = annotateTransitions op1cut polygon2
        op2cutAnnotated = annotateTransitions op2cut polygon1

        (op1cutGraph, op1entersOp2) = buildGraph op1cutAnnotated
        (op2cutGraph, _op2entersOp1) = buildGraph op2cutAnnotated

        initialState = ReconstructionState
            { _currentPos = bugError "weilerAthertonUnion" "This should be filled when needed by the algorithm, and not used otherwise!"
            , _unvisitedEnteringPoints = op1entersOp2
            , _graph1Active = bugError "weilerAthertonUnion" "This should be filled when needed by the algorithm, and not used otherwise!"
            , _graph1 = op1cutGraph
            , _graph2 = op2cutGraph
            }

        (polygons, _finalState) = runState reconstructAllUnionPolygons initialState

    in polygons

-- | All polygons resulting of the intersection of the source polygons.
intersectionOfTwoPolygons :: Polygon -> Polygon -> [Polygon]
intersectionOfTwoPolygons = weilerAthertonUnion
