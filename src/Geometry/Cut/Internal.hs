-- | __INTERNAL MODULE__, contents may change arbitrarily

{-# LANGUAGE MultiWayIf #-}

module Geometry.Cut.Internal where



import           Data.List
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Set   as S

import Geometry.Core

-- | Cut a finite piece of paper in one or two parts with an infinite line
cutLine :: Line -> Line -> CutLine
cutLine scissors paper = case intersectionLL scissors paper of
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
            (cutAll scissors
                (polygonEdges polygon)))

-- Generate a list of all the edges of a polygon, extended with additional
-- points on the edges that are crossed by the scissors.
cutAll :: Line -> [Line] -> [CutLine]
cutAll scissors edges = map (cutLine scissors) edges

createEdgeGraph :: Line -> PolygonOrientation -> [CutLine] -> CutEdgeGraph
createEdgeGraph scissors orientation allCuts = buildGraph (addCutEdges ++ addOriginalPolygon)
  where
    addCutEdges = newCutsEdgeGraph scissors orientation allCuts
    addOriginalPolygon = polygonEdgeGraph allCuts

buildGraph :: [CutEdgeGraph -> CutEdgeGraph] -> CutEdgeGraph
buildGraph = foldl' (\graph insertEdge -> insertEdge graph) (CutEdgeGraph mempty)

newCutsEdgeGraph :: Line -> PolygonOrientation -> [CutLine] -> [CutEdgeGraph -> CutEdgeGraph]
newCutsEdgeGraph scissors@(Line scissorsStart _) orientation cuts = go cutPointsSorted
  where
    go :: [NormalizedCut] -> [CutEdgeGraph -> CutEdgeGraph]
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

    go bad
      = bugError $ unlines
          [ "Expecting patterns to be exhaustive, but apparently it's not."
          , show bad ]

    cutPointsSorted :: [NormalizedCut]
    cutPointsSorted = sortOn scissorCoordinate (normalizeCuts scissors orientation cuts)

    -- How far ahead/behind the start of the line is the point?
    --
    -- In mathematical terms, this yields the coordinate of a point in the
    -- 1-dimensional vector space that is the scissors line.
    scissorCoordinate :: NormalizedCut -> Double
    scissorCoordinate nc = case nc of
        Entering x -> positionAlongScissor x
        Exiting x -> positionAlongScissor x
        Touching x -> positionAlongScissor x
        AlongEdge x y -> min (positionAlongScissor x) (positionAlongScissor y)
      where
        positionAlongScissor p = dotProduct (vectorOf scissors) (vectorOf (Line scissorsStart p))

-- A polygon can be described by an adjacency list of corners to the next
-- corner. A cut simply introduces two new corners (of polygons to be) that
-- point to each other.
polygonEdgeGraph :: [CutLine] -> [CutEdgeGraph -> CutEdgeGraph]
polygonEdgeGraph cuts = case cuts of
    Cut p x q : rest -> (p --> x) : (x --> q) : polygonEdgeGraph rest
    NoCut p q : rest -> (p --> q) : polygonEdgeGraph rest
    []               -> []

-- Insert a (corner -> corner) edge
(-->) :: Vec2 -> Vec2 -> CutEdgeGraph -> CutEdgeGraph
(k --> v) edgeGraph@(CutEdgeGraph edgeMap) = case M.lookup k edgeMap of
    _ | k == v    -> edgeGraph
    Nothing       -> CutEdgeGraph (M.insert k [v] edgeMap)
    Just vs -> CutEdgeGraph (M.insert k (v:vs) edgeMap)

newtype CutEdgeGraph = CutEdgeGraph (Map Vec2 [Vec2])
    deriving (Eq, Ord)

instance Show CutEdgeGraph where
    show (CutEdgeGraph m) = unlines
        ("CutEdgeGraph" : (("    " ++) . (\(k, v) -> show k ++ " -> " ++ show v) <$> M.toList m))

-- Given a list of corners that point to other corners, we can reconstruct
-- all the polygons described by them by finding the smallest cycles, i.e.
-- cycles that do not contain other (parts of the) adjacency map.
--
-- Starting at an arbitrary point, we can extract a single polygon by
-- following such a minimal cycle; iterating this algorithm until the entire
-- map has been consumed yields all the polygons.
reconstructPolygons :: PolygonOrientation -> CutEdgeGraph -> [Polygon]
reconstructPolygons orientation edgeGraph@(CutEdgeGraph graphMap) = case M.lookupMin graphMap of
    Nothing -> []
    Just (edgeStart, _end) -> case poly of
        Polygon (_:_) -> poly : reconstructPolygons orientation edgeGraph'
        _otherwise -> bugError $ unlines
            [ "Empty Polygon constructed from edge graph."
            , "This means that the edge graph cannot be deconstructed further:"
            , show edgeGraph ]
      where (poly, edgeGraph') = extractSinglePolygon orientation edgeStart edgeGraph

-- | Extract a single polygon from an edge map by finding a minimal circular
-- connection.
extractSinglePolygon
    :: PolygonOrientation
    -> Vec2                    -- ^ Starting point
    -> CutEdgeGraph            -- ^ Edge map
    -> (Polygon, CutEdgeGraph) -- ^ Extracted polygon and remaining edge map
extractSinglePolygon orientation = go Nothing S.empty
  where
    go lastPivot visited pivot edgeGraph@(CutEdgeGraph edgeMap)
      = case M.lookup pivot edgeMap of
            _ | S.member pivot visited -> (Polygon [], edgeGraph)
            Nothing -> (Polygon [], edgeGraph)
            Just [] -> (Polygon [], edgeGraph)
            Just [next] ->
                let (Polygon rest, edgeGraph') = go
                        (Just pivot)
                        (S.insert pivot visited)
                        next
                        (CutEdgeGraph (M.delete pivot edgeMap))
                in (Polygon (pivot:rest), edgeGraph')
            Just (toVertices@(next1:_)) ->
                let useAsNext = case lastPivot of
                        Nothing -> next1 -- arbitrary starting point WLOG
                        Just from ->
                            let leftness end = angleOfLine (Line pivot from) -. angleOfLine (Line pivot end)
                                rightness end = negateV (leftness end)
                                pickNextVertex = minimumBy $ comparing $ case orientation of
                                    PolygonPositive -> leftness
                                    PolygonNegative -> rightness
                            in  pickNextVertex (filter (/= from) toVertices)
                    otherVertices = toVertices \\ [useAsNext]
                    (Polygon rest, edgeGraph') = go
                        (Just pivot)
                        (S.insert pivot visited)
                        useAsNext
                        (CutEdgeGraph (M.insert pivot otherVertices edgeMap))
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
        | ty `elem` [LOR, ROL] = normalizedCutFor ty x : go cuts
        -- cuts through vertex need to be merged with the next cut
        -- they come in pairs, or even more (for cuts along a line)
        | ty `elem` [LOO, ROO] = mergeCutsThroughVertex (x, ty) cuts
        -- Everything else is an error (OOX: this should be prevented by rotateToEntryPoint/mergeCutsThroughVertex)
        | otherwise = bugError $ unlines
            [ "Found invalid cut type " ++ show ty
            , "Maybe rotateToEntryPoint did not work as expected?" ]

    mergeCutsThroughVertex :: (Vec2, CutType) -> [(Vec2, CutType)] -> [NormalizedCut]
    mergeCutsThroughVertex (x, ty) cuts = case (ty, cuts) of
        -- A cut through a vertex results in two entries, merge them into one cut.
        -- We can ignore the second cut point, it should be identical to x
        (LOO, (_, OOR) : rest) -> normalizedCutFor LOR x : go rest
        (LOO, (_, OOL) : rest) -> normalizedCutFor LOL x : go rest
        (ROO, (_, OOR) : rest) -> normalizedCutFor ROR x : go rest
        (ROO, (_, OOL) : rest) -> normalizedCutFor ROL x : go rest
        -- A cut along a line is more complicated: We ignore the cut in between
        -- that is completely along the line, and follow the line
        -- until we reach an edge that leads away from the cut.
        (_, (_, OOO) : rest) -> followCutAlongLine x rest
        other -> bugError ("Encountered unexpected cut type when merging cuts through vertex: " ++ show other)

    followCutAlongLine :: Vec2 -> [(Vec2, CutType)] -> [NormalizedCut]
    followCutAlongLine x ((y, yTy) : rest) = case yTy of
        -- Another cut along the line, skip again
        OOO -> followCutAlongLine x rest
        -- Found the edge that leads away from the cut line
        OOL -> AlongEdge x y : go rest
        OOR -> AlongEdge x y : go rest
        -- Either the function was called with the wrong input, or the polygon was inconsistent
        _ -> bugError "Tried to follow cut along line, but there is no valid option to follow."
    followCutAlongLine _ [] = bugError "Tried to follow cut along line, but there is nothing to follow"

    normalizedCutFor :: CutType -> Vec2 -> NormalizedCut
    normalizedCutFor LOR = case orientation of
        PolygonPositive -> Entering
        PolygonNegative -> Exiting
    normalizedCutFor ROL = case orientation of
        PolygonNegative -> Entering
        PolygonPositive -> Exiting
    normalizedCutFor LOL = Touching
    normalizedCutFor ROR = Touching
    normalizedCutFor other = bugError $ unlines
        [ "Can only normalize cuts that cross the line, found: " ++ show other
        , "Maybe mergeCutsThroughVertex should be applied?" ]

    rotateToEntryPoint [] = []
    rotateToEntryPoint (c@(v, ty) : cs)
        | ty `elem` [LOR, ROL, LOO, ROO] = c:cs
        | otherwise = rotateToEntryPoint (cs ++ [c])

data NormalizedCut
    = Entering Vec2
    | Exiting Vec2
    | Touching Vec2
    | AlongEdge Vec2 Vec2
    deriving (Show)

classifyCut :: Line -> CutLine -> Maybe (Vec2, CutType)
classifyCut _ NoCut{} = Nothing
classifyCut scissors (Cut l x r)
  = Just $ case (sideOfScissors scissors l, sideOfScissors scissors r) of
        (LeftOfLine,     LeftOfLine)     -> (x, LOL)
        (LeftOfLine,     RightOfLine)    -> (x, LOR)
        (RightOfLine,    LeftOfLine)     -> (x, ROL)
        (RightOfLine,    RightOfLine)    -> (x, ROR)
        (DirectlyOnLine, DirectlyOnLine) -> (x, OOO)
        (DirectlyOnLine, LeftOfLine)     -> (x, OOL)
        (DirectlyOnLine, RightOfLine)    -> (x, OOR)
        (LeftOfLine,     DirectlyOnLine) -> (x, LOO)
        (RightOfLine,    DirectlyOnLine) -> (x, ROO)

startPoint :: CutLine -> Vec2
startPoint (NoCut p _) = p
startPoint (Cut p _ _) = p

endPoint :: CutLine -> Vec2
endPoint (NoCut _ q) = q
endPoint (Cut _ _ q) = q

sideOfScissors :: Line -> Vec2 -> SideOfLine
sideOfScissors scissors@(Line scissorsStart _) p
  = let scissorsCrossPoint = det (vectorOf scissors) (vectorOf (Line scissorsStart p))
    in case compare scissorsCrossPoint 0 of
        LT -> RightOfLine
        EQ -> DirectlyOnLine
        GT -> LeftOfLine

data CutLine
    = NoCut Vec2 Vec2
        -- ^ (start, end). No cut has occurred, i.e. the cutting line did not
        -- intersect with the object.
    | Cut Vec2 Vec2 Vec2
        -- ^ (start, cut, end). The input was divided in two lines.
    deriving (Eq, Ord, Show)

data SideOfLine = LeftOfLine | DirectlyOnLine | RightOfLine
    deriving (Eq, Ord, Show)

-- | Nomenclature: Left/On/Right relative to scissors. LOR means that the cut is
-- on the scissors, the edge leading to the cut comes from the left of it, and
-- the outgoing edge extends to the right.
data CutType = LOL | LOO | LOR | OOL | OOO | OOR | ROL | ROO | ROR
    deriving (Eq, Ord, Show)
