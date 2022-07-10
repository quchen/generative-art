-- | __INTERNAL MODULE__, not exposed from the package.
module Geometry.Algorithms.Clipping.Internal (
      cutLineWithLine
    , CutLine(..)
    , cutPolygon

    , LineType(..)
    , clipPolygonWithLine
    , clipPolygonWithLineSegment
) where


import           Data.List
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe
import           Data.Ord
import           Data.Set   (Set)
import qualified Data.Set   as S

import Geometry.Core
import Util



-- | Directed graph from each vertex to the next, so that following the chain of
-- pointers allows reconstruction of certain properties. Subdividing polygons is
-- done by finding minimal cycles, for example.
newtype EdgeGraph = EdgeGraph (Map Vec2 (Set Vec2))
    deriving (Eq, Ord)

instance Show EdgeGraph where
    show (EdgeGraph m) = unlines
        ("EdgeGraph" : ["    " ++ show k ++ " --> " ++ show v | (k,v) <- M.toList m])

instance Semigroup EdgeGraph where
    EdgeGraph g1 <> EdgeGraph g2 = EdgeGraph (M.unionWith (<>) g1 g2)

instance Monoid EdgeGraph where
    mempty = EdgeGraph mempty

-- | An edge to be inserted into an EdgeGraph. Technically just a 'Line', but local
-- to the module so it can easily be extended by edge labels.
data Edge = Edge Vec2 Vec2
    deriving (Eq, Ord, Show)

-- | Pretty synonym for 'Edge'.
(-->) :: Vec2 -> Vec2 -> Edge
(-->) = Edge

data CutLine
    = NoCut Vec2 Vec2
        -- ^ (start, end). No cut has occurred, i.e. the cutting line did not
        -- intersect with the object.
    | Cut Vec2 Vec2 Vec2
        -- ^ (start, cut, end). The input was divided in two lines.
    deriving (Eq, Ord, Show)

data NormalizedCut
    = Entering Vec2
    | Exiting Vec2
    | Touching Vec2
    | AlongEdge Vec2 Vec2
    deriving (Eq, Ord, Show)

-- | Cut a finite piece of paper in one or two parts with an infinite scissors line
-- (depending on whether the scissors miss the line or not).
cutLineWithLine :: Line -> Line -> CutLine
cutLineWithLine scissors paper = case intersectionLL scissors paper of
    IntersectionReal p           -> cut p
    IntersectionVirtualInsideR p -> cut p
    Collinear _                  -> cut paperStart -- any point is good enough
    _otherwise                   -> noCut
  where
    Line paperStart paperEnd = paper
    cut p = Cut paperStart p paperEnd
    noCut = NoCut paperStart paperEnd

-- | Cut a polygon in multiple pieces with a line.
--
-- For convex polygons, the result is either just the polygon (if the line
-- misses) or two pieces. Concave polygons can in general be divided in
-- arbitrarily many pieces.
--
-- <<docs/geometry/clipping/3_complicated.svg>>
cutPolygon :: Line -> Polygon -> [Polygon]
cutPolygon scissors polygon =
    -- The idea here is as follows:
    --
    -- A polygon can be seen as a cyclic graph where each corner points to the
    --next. We can reconstruct the polygon by looking for a cycle.
    --
    -- We can use this idea to cut the polygon: a cut introduces new edges into
    -- our corner graph, going from the start of the cut to the end. We can then
    -- get all the pieces of the cut by looking for all the cycles in that graph.
    reconstructPolygons
        (polygonOrientation polygon)
        (createEdgeGraph scissors (polygonOrientation polygon)
            (map (cutLineWithLine scissors)
                 (polygonEdges polygon)))

createEdgeGraph :: Line -> PolygonOrientation -> [CutLine] -> EdgeGraph
createEdgeGraph scissors orientation allCuts = buildGraph (addCutEdges ++ addOriginalPolygon)
  where
    addCutEdges = cutsToEdges scissors orientation allCuts
    addOriginalPolygon = polygonToEdges allCuts

buildGraph :: Foldable f => f Edge -> EdgeGraph
buildGraph = foldl' (\graph@(EdgeGraph g) (Edge start end) -> if start == end then graph else EdgeGraph (M.insertWith S.union start (S.singleton end) g)) mempty

cutsToEdges :: Line -> PolygonOrientation -> [CutLine] -> [Edge]
cutsToEdges scissors orientation cuts = go (cutPointsSorted scissors orientation cuts)
  where
    go :: [NormalizedCut] -> [Edge]
    go [] = []

    -- Default path: Scissors enter and exit again
    go (Entering p : Exiting q : rest)
      = (p --> q) : (q --> p) : go rest

    -- Touching the polygon along a line, and exiting it afterwards:
    -- Looks like we did enter along the line.
    go (AlongEdge _ p : Exiting q : rest)
      = go (Entering p : Exiting q : rest)

    -- Touching the polygon, but not entering it: Just ignore
    go (Touching _ : rest)
      = go rest

    -- Touching the polygon along two successive lines: Merge them
    -- (This should already be merged, but doesn't harm here)
    go (AlongEdge p _ : AlongEdge _ q : rest)
      = go (AlongEdge p q : rest)

    -- Going along an edge without entering or exiting: Ignore the edge
    go (AlongEdge _ _ : rest)
      = go rest

    -- Touching a point while inside the polygon:
    -- Treat it like entering, exiting, and re-entering
    go (Entering p : Touching q : rest)
      = go (Entering p : Exiting q : Entering q : rest)

    -- Same here, but we cannot re-enter, need to first follow along the cut
    go (Entering p : AlongEdge q r : rest)
      = go (Entering p : Exiting q : AlongEdge q r : rest)

    -- I encountered this bug a few times now: Two cuts in the wrong order.
    -- I haven't debugged this to find out where it goes wrong, but it looks
    -- like this is a pretty safe workaround.
    go [Exiting p, Entering q]
      = go [Entering p, Exiting q]

    go bad
      = bugError "Cut.Internal.newCutsGraphEdges" $ unlines
          [ "Expecting patterns to be exhaustive, but apparently it's not."
          , "Bad portion: " ++ show bad
          , "Full list of cut lines: " ++ show (cutPointsSorted scissors orientation cuts) ]

-- | Sort cut points by location on the scissors
cutPointsSorted :: Line -> PolygonOrientation -> [CutLine] -> [NormalizedCut]
cutPointsSorted scissors orientation cuts = sortOn (scissorCoordinate scissors) (normalizeCuts scissors orientation cuts)

-- How far ahead/behind the start of the line is the point?
--
-- In mathematical terms, this yields the coordinate of a point in the
-- 1-dimensional vector space that is the scissors line.
scissorCoordinate :: Line -> NormalizedCut -> Double
scissorCoordinate scissors@(Line scissorsStart _) nc = case nc of
    Entering x -> positionAlongScissor x
    Exiting x -> positionAlongScissor x
    Touching x -> positionAlongScissor x
    AlongEdge x y -> min (positionAlongScissor x) (positionAlongScissor y)
    where
    positionAlongScissor p = dotProduct (vectorOf scissors) (vectorOf (Line scissorsStart p))

-- A polygon can be described by an adjacency list of corners to the next
-- corner. A cut simply introduces two new corners (of polygons to be) that
-- point to each other.
polygonToEdges :: [CutLine] -> [Edge]
polygonToEdges cuts = case cuts of
    Cut p x q : rest -> (p --> x) : (x --> q) : polygonToEdges rest
    NoCut p q : rest -> (p --> q) : polygonToEdges rest
    []               -> []

-- | Given a list of corners that point to other corners, we can reconstruct all
-- the polygons described by them by finding the smallest cycles, i.e. cycles that
-- do not contain other (parts of the) adjacency map.
--
-- Starting at an arbitrary point, we can extract a single polygon by
-- following such a minimal cycle; iterating this algorithm until the entire
-- map has been consumed yields all the polygons.
reconstructPolygons :: PolygonOrientation -> EdgeGraph -> [Polygon]
reconstructPolygons orientation edgeGraph@(EdgeGraph graphMap) = case M.lookupMin graphMap of
    Nothing -> []
    Just (edgeStart, _end) -> case poly of
        Polygon (_:_) -> poly : reconstructPolygons orientation edgeGraph'
        _otherwise -> bugError "Cut.Internal.reconstructPolygons" $ unlines
            [ "Empty Polygon constructed from edge graph."
            , "This means that the edge graph cannot be deconstructed further:"
            , show edgeGraph ]
      where (poly, edgeGraph') = extractSinglePolygon orientation edgeStart edgeGraph

-- | Extract a single polygon from an edge map by finding a minimal circular
-- connection.
extractSinglePolygon
    :: PolygonOrientation
    -> Vec2                    -- ^ Starting point
    -> EdgeGraph            -- ^ Edge map
    -> (Polygon, EdgeGraph) -- ^ Extracted polygon and remaining edge map
extractSinglePolygon orientation = go Nothing S.empty
  where
    go lastPivot visited pivot edgeGraph@(EdgeGraph edgeMap) = case M.lookup pivot edgeMap of

        -- We were already here: terminate (TODO: shouldn’t this be an error?)
        _ | S.member pivot visited -> (Polygon [], edgeGraph)

        -- The pivot is not in the edge map: terminate. (TODO: shouldn’t this be an error?)
        Nothing -> (Polygon [], edgeGraph)

        Just toVertices -> case S.minView toVertices of
            -- The pivot is there, but does not point anywhere. (TODO: shouldn’t this be an error?)
            Nothing -> (Polygon [], edgeGraph)

            -- The pivot points to a single target; follow the pointer
            Just (next, nothingLeft) | S.null nothingLeft ->
                let (Polygon rest, edgeGraph') = go
                        (Just pivot)
                        (S.insert pivot visited)
                        next
                        (EdgeGraph (M.delete pivot edgeMap))
                in (Polygon (pivot:rest), edgeGraph')

            -- The pivot points to multiple targets; follow the direction of the smallest loop
            Just (next1, _) ->
                let useAsNext = case lastPivot of
                        Nothing -> next1 -- There was no previous pivot; pick an arbitrary starting point WLOG
                        Just from ->
                            let leftness, rightness :: Vec2 -> Angle
                                leftness end = normalizeAngle (rad 0) (angleOfLine (Line pivot from) -. angleOfLine (Line pivot end))
                                rightness end = negateV (leftness end)
                                pickNextVertex = minimumBy $ comparing $ case orientation of
                                    -- TODO: comparing by angles is flaky because of their modular arithmetic.
                                    -- leftness/rightness should be rewritten in terms of 'cross', which allows
                                    -- judging whether a vector is left/right of another much better.
                                    -- The 'getRad' was just put here quickly so the 'Ord Angle' instance
                                    -- could be removed.
                                    PolygonPositive -> getRad . leftness
                                    PolygonNegative -> getRad . rightness
                            in pickNextVertex (S.delete from toVertices)
                    otherVertices = S.delete useAsNext toVertices
                    (Polygon rest, edgeGraph') = go
                        (Just pivot)
                        (S.insert pivot visited)
                        useAsNext
                        (EdgeGraph (M.insert pivot otherVertices edgeMap))
                in (Polygon (pivot:rest), edgeGraph')

normalizeCuts :: Line -> PolygonOrientation -> [CutLine] -> [NormalizedCut]
normalizeCuts _ _ [] = []
normalizeCuts scissors orientation cutLines =
    go (rotateToEntryPoint (mapMaybe (classifyCut scissors) cutLines))
  where
    go :: [(Vec2, CutType)] -> [NormalizedCut]
    go [] = []
    go ((x, ty) : cuts)
        -- regular cuts, just collect them
        | ty `elem` [LR, RL] = normalizedCutFor ty x : go cuts

        -- cuts through vertex need to be merged with the next cut
        -- they come in pairs, or even more (for cuts along a line)
        | ty `elem` [LO, RO] = mergeCutsThroughVertex (x, ty) cuts

        -- Everything else is an error (OX: this should be prevented by rotateToEntryPoint/mergeCutsThroughVertex)
        | otherwise = bugError "Cut.Internal.normalizeCuts.go" $ unlines
            [ "Found invalid cut type " ++ show ty
            , "Maybe rotateToEntryPoint did not work as expected?" ]

    mergeCutsThroughVertex :: (Vec2, CutType) -> [(Vec2, CutType)] -> [NormalizedCut]
    mergeCutsThroughVertex (x, ty) cuts = case (ty, cuts) of
        -- A cut through a vertex results in two entries, merge them into one cut.
        -- We can ignore the second cut point, it should be identical to x
        (LO, (_, OR) : rest) -> normalizedCutFor LR x : go rest
        (RO, (_, OL) : rest) -> normalizedCutFor RL x : go rest
        (LO, (_, OL) : rest) -> Touching x : go rest
        (RO, (_, OR) : rest) -> Touching x : go rest
        -- A cut along a line is more complicated: We ignore the cut in between
        -- that is completely along the line, and follow the line
        -- until we reach an edge that leads away from the cut.
        (_, (_, OO) : rest) -> followCutAlongLine x rest
        other -> bugError "Cut.Internal.normalizeCuts.mergeCutsThroughVertex" ("Encountered unexpected cut type when merging cuts through vertex: " ++ show other)

    followCutAlongLine :: Vec2 -> [(Vec2, CutType)] -> [NormalizedCut]
    followCutAlongLine x ((y, yTy) : rest) = case yTy of
        -- Another cut along the line, skip again
        OO -> followCutAlongLine x rest
        -- Found the edge that leads away from the cut line
        OL -> AlongEdge x y : go rest
        OR -> AlongEdge x y : go rest
        -- Either the function was called with the wrong input, or the polygon was inconsistent
        _ -> bugError "Cut.Internal.normalizeCuts.followCutAlongLine" "Tried to follow cut along line, but there is no valid option to follow."
    followCutAlongLine _ [] = bugError "Cut.Internal.normalizeCuts.followCutAlongLine" "Tried to follow cut along line, but there is nothing to follow"

    normalizedCutFor :: CutType -> Vec2 -> NormalizedCut
    normalizedCutFor LR = case orientation of
        PolygonPositive -> Entering
        PolygonNegative -> Exiting
    normalizedCutFor RL = case orientation of
        PolygonNegative -> Entering
        PolygonPositive -> Exiting
    normalizedCutFor other = bugError "Cut.Internal.normalizeCuts.normalizedCutFor" $ unlines
        [ "Can only normalize cuts that cross the line, found: " ++ show other
        , "Maybe mergeCutsThroughVertex should be applied?" ]

    rotateToEntryPoint [] = []
    rotateToEntryPoint (c@(_, ty) : cs)
        | ty `elem` [LR, RL, LO, RO] = c:cs
        | otherwise = rotateToEntryPoint (cs ++ [c])

classifyCut :: Line -> CutLine -> Maybe (Vec2, CutType)
classifyCut _ NoCut{} = Nothing
classifyCut scissors (Cut l x r)
  = Just $ case (sideOfScissors scissors l, sideOfScissors scissors r) of
        (LeftOfLine,     RightOfLine)    -> (x, LR)
        (RightOfLine,    LeftOfLine)     -> (x, RL)
        (DirectlyOnLine, DirectlyOnLine) -> (x, OO)
        (DirectlyOnLine, LeftOfLine)     -> (x, OL)
        (DirectlyOnLine, RightOfLine)    -> (x, OR)
        (LeftOfLine,     DirectlyOnLine) -> (x, LO)
        (RightOfLine,    DirectlyOnLine) -> (x, RO)
        other -> bugError "Cut.Internal.classifyCut" ("Unexpected cut that cannot be classified: " ++ show other)

sideOfScissors :: Line -> Vec2 -> SideOfLine
sideOfScissors scissors@(Line scissorsStart _) p
  = let scissorsCrossPoint = cross (vectorOf scissors) (vectorOf (Line scissorsStart p))
    in case compare scissorsCrossPoint 0 of
        LT -> RightOfLine
        EQ -> DirectlyOnLine
        GT -> LeftOfLine

data SideOfLine = LeftOfLine | DirectlyOnLine | RightOfLine
    deriving (Eq, Ord, Show)

-- | Nomenclature: Left/On/Right relative to scissors. LR means that the edge
-- leading to the cut comes from the left of the scissors, and the outgoing
-- edge extends to the right. LO means that the edge leading to the cut comes
-- from the left, but we cut exactly through the vertex.
data CutType = LO | LR | OL | OO | OR | RL | RO
    deriving (Eq, Ord, Show)

data LineType = LineInsidePolygon | LineOutsidePolygon
    deriving (Eq, Ord, Show)

-- | Classify lines on the scissors as being inside or outside the polygon.
clipPolygonWithLine :: Polygon -> Line -> [(Line, LineType)]
clipPolygonWithLine polygon scissors = reconstruct normalizedCuts

  where
    allCuts = map (cutLineWithLine scissors) (polygonEdges polygon)
    orientation = polygonOrientation polygon
    normalizedCuts = cutPointsSorted scissors orientation allCuts

    -- Happy path
    reconstruct (Entering start : rest@((Exiting end) : _)) = (Line start end, LineInsidePolygon) : reconstruct rest
    reconstruct (e@Entering{} : AlongEdge{} : rest) = reconstruct (e:rest) -- We ignore the alongEdge part here, much like the 'Touching' cases
    reconstruct (Exiting start : rest@(Entering end : _)) = (Line start end, LineOutsidePolygon) : reconstruct rest
    reconstruct (e@Exiting{} : AlongEdge{} : rest) = reconstruct (e:rest) -- We ignore the alongEdge part here, much like the 'Touching' cases
    reconstruct [Exiting{}] = []

    -- Unhappy path, cutting along edges: do some lookahead to decide whether we’re in our out
    reconstruct (AlongEdge start end : rest@(AlongEdge{} : _)) = (Line start end, LineInsidePolygon) : reconstruct rest
    reconstruct (AlongEdge start end : rest@(Entering{} : _)) = (Line start end, LineOutsidePolygon) : reconstruct rest
    reconstruct (AlongEdge start end : rest@(Exiting{} : _)) = (Line start end, LineInsidePolygon) : reconstruct rest
    reconstruct [AlongEdge{}] = [] -- This might also be declared an error, but in the name of sanity let’s just say this does nothing

    -- Ignore touch points: simply continue the line
    reconstruct (e@Entering{} : Touching{} : rest) = reconstruct (e:rest)
    reconstruct (e@Exiting{} : Touching{} : rest) = reconstruct (e:rest)
    reconstruct (along@AlongEdge{} : Touching{} : rest) = reconstruct (along:rest)
    reconstruct (Touching{} : rest) = reconstruct rest

    reconstruct (Entering{} : Entering {} : _) = bugError "Cut.Internal.clipPolygonWithLine" "Double enter"
    reconstruct (Exiting{} : Exiting {} : _) = bugError "Cut.Internal.clipPolygonWithLine" "Double exit"
    reconstruct [Entering{}] = bugError "Cut.Internal.clipPolygonWithLine" "Standalone enter"
    reconstruct [] = [] -- Input was empty to begin with, otherwise one of the other cases happens

clipPolygonWithLineSegment :: Polygon -> Line -> [(Line, LineType)]
clipPolygonWithLineSegment polygon scissors@(Line start end) = reconstructSegments sortedPoints
  where
    allIntersectionPoints =
        [ p
        | edge <- polygonEdges polygon
        , IntersectionReal p <- pure (intersectionLL edge scissors)
        ]
    sortedPoints = sortOn (\p -> direction scissors `dotProduct` (p -. start)) ([start, end] ++ allIntersectionPoints)
    reconstructSegments = \case
        [] -> []
        [_] -> []
        a : b : xs ->
            let segment = Line a b
                lineType = if ((a +. b) /. 2) `pointInPolygon` polygon
                    then LineInsidePolygon
                    else LineOutsidePolygon
            in  (segment, lineType) : reconstructSegments (b : xs)
