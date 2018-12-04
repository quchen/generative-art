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
    (p, IntersectionReal)           -> cut p
    (p, IntersectionVirtualInsideR) -> cut p
    (_, IntersectionVirtualInsideL) -> noCut
    (_, IntersectionVirtual)        -> noCut
  where
    Line paperStart paperEnd = paper
    cut p = Cut paperStart p paperEnd
    noCut = NoCut paperStart paperEnd

-- | Used in the implementation of a multimap where each entry can have one or
-- two values.
data OneOrTwo a = One a | Two a a
    deriving (Eq, Ord, Show)

-- | Orientation of a polygon
data Orientation = Positive | Negative
    deriving (Eq, Ord, Show)

-- | Cut a polygon in multiple pieces with a line.
--
-- For convex polygons, the result is either just the polygon (if the line
-- misses) or two pieces. Concave polygons can in general be divided in
-- arbitrarily many pieces.
cutPolygon :: Line -> Polygon -> [Polygon]
cutPolygon scissors polygon =
    reconstructPolygons
        (createEdgeGraph scissors (polygonOrientation polygon)
            (cutAll scissors
                (polygonEdges polygon)))

polygonOrientation :: Polygon -> Orientation
polygonOrientation polygon
    | signedPolygonArea polygon >= Area 0 = Positive
    | otherwise                           = Negative

-- Generate a list of all the edges of a polygon, extended with additional
-- points on the edges that are crossed by the scissors.
cutAll :: Line -> [Line] -> [CutLine]
cutAll scissors edges = map (cutLine scissors) edges

createEdgeGraph :: Line -> Orientation -> [CutLine] -> CutEdgeGraph
createEdgeGraph scissors orientation allCuts = buildGraph (addCutEdges ++ addOriginalPolygon)
  where
    addCutEdges = newCutsEdgeGraph scissors orientation allCuts
    addOriginalPolygon = polygonEdgeGraph allCuts

buildGraph :: [CutEdgeGraph -> CutEdgeGraph] -> CutEdgeGraph
buildGraph = foldl' (\graph insertEdge -> insertEdge graph) mempty

newCutsEdgeGraph :: Line -> Orientation -> [CutLine] -> [CutEdgeGraph -> CutEdgeGraph]
newCutsEdgeGraph scissors@(Line scissorsStart _) orientation cuts = go cutPointsSorted
  where
    go ((p, pTy) : (q, qTy) : rest)
      = let isSource = isSourceType orientation
            isTarget = isTargetType orientation
        in if
            | isSource pTy && isTarget qTy -> (p --> q) : (q --> p) : go rest
            | isTarget pTy -> bugError "Target without source"
            | isSource pTy && isSource qTy -> go ((q, qTy) : rest)
            | otherwise -> bugError ("Bad source/target (" ++ show (isSource pTy) ++ "/" ++ show (isTarget pTy) ++ ")")
    go (_:_) = bugError "Unpaired cut point"
    go [] = []

    cutPointsSorted :: [(Vec2, CutType)]
    cutPointsSorted = sortOn (scissorCoordinate . fst) (M.toList recordedNeighbours)

    _cuts0 : cuts1 : cuts2 : _ = iterate tail (cycle cuts)

    recordedNeighbours :: Map Vec2 CutType
    recordedNeighbours
      = M.fromList (catMaybes (zipWith3 (classifyCut scissors)
                                        cuts
                                        cuts1
                                        cuts2 ))

    -- How far ahead/behind the start of the line is the point?
    --
    -- In mathematical terms, this yields the coordinate of a point in the
    -- 1-dimensional vector space that is the scissors line.
    scissorCoordinate :: Vec2 -> Double
    scissorCoordinate p = dotProduct (vectorOf scissors) (vectorOf (Line scissorsStart p))

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
    Nothing       -> CutEdgeGraph (M.insert k (One v) edgeMap)
    Just (One v') -> CutEdgeGraph (M.insert k (Two v v') edgeMap)
    Just Two{}    -> bugError "Third edge in cutting algorithm"

newtype CutEdgeGraph = CutEdgeGraph (Map Vec2 (OneOrTwo Vec2))
    deriving (Eq, Ord, Show)

instance Semigroup CutEdgeGraph where
    CutEdgeGraph g1 <> CutEdgeGraph g2 = CutEdgeGraph (g1 <> g2)

instance Monoid CutEdgeGraph where
    mempty = CutEdgeGraph mempty

-- Given a list of corners that point to other corners, we can reconstruct
-- all the polygons described by them by finding the smallest cycles, i.e.
-- cycles that do not contain other (parts of the) adjacency map.
--
-- Starting at an arbitrary point, we can extract a single polygon by
-- following such a minimal cycle; iterating this algorithm until the entire
-- map has been consumed yields all the polygons.
reconstructPolygons :: CutEdgeGraph -> [Polygon]
reconstructPolygons edgeGraph@(CutEdgeGraph graphMap) = case M.lookupMin graphMap of
    Nothing -> []
    Just (edgeStart, _end) ->
        let (poly, edgeGraph') = extractSinglePolygon edgeStart edgeGraph
        in poly : reconstructPolygons edgeGraph'

-- | Extract a single polygon from an edge map by finding a minimal circular
-- connection.
extractSinglePolygon
    :: Vec2                    -- ^ Starting point
    -> CutEdgeGraph            -- ^ Edge map
    -> (Polygon, CutEdgeGraph) -- ^ Extracted polygon and remaining edge map
extractSinglePolygon = go Nothing S.empty
  where
    go lastPivot visited pivot edgeGraph@(CutEdgeGraph edgeMap)
      = case M.lookup pivot edgeMap of
            _ | S.member pivot visited -> (Polygon [], edgeGraph)
            Nothing -> (Polygon [], edgeGraph)
            Just (One next) ->
                let (Polygon rest, edgeGraph') = go
                        (Just pivot)
                        (S.insert pivot visited)
                        next
                        (CutEdgeGraph (M.delete pivot edgeMap))
                in (Polygon (pivot:rest), edgeGraph')
            Just (Two next1 next2) ->
                let useAsNext = case lastPivot of
                        Nothing -> next1 -- arbitrary starting point WLOG
                        Just from -> let forwardness end = dotProduct
                                             (direction (Line from pivot))
                                             (direction (Line pivot end))
                                     in minimumBy (comparing forwardness)
                                                  (filter (/= from) [next1, next2])
                    unusedNext | useAsNext == next1 = next2
                               | otherwise = next1
                    (Polygon rest, edgeGraph') = go
                        (Just pivot)
                        (S.insert pivot visited)
                        useAsNext
                        (CutEdgeGraph (M.insert pivot (One unusedNext) edgeMap))
                in (Polygon (pivot:rest), edgeGraph')

classifyCut :: Line -> CutLine -> CutLine -> CutLine -> Maybe (Vec2, CutType)
classifyCut _ _ NoCut{} _ = Nothing
-- -- OOO
-- classifyCut _ Cut{} (Cut _ x _) Cut{} = Just (x, OOO)
-- -- OOX
-- classifyCut scissors Cut{} (Cut _ x _) r
--   = case sideOfScissors scissors (endPoint r) of
--         LeftOfLine     -> Just (x, OOL)
--         RightOfLine    -> Just (x, OOR)
--         DirectlyOnLine -> Just (x, OOO)
-- -- XOO
-- classifyCut scissors l (Cut _ x _) Cut{}
--   = case sideOfScissors scissors (startPoint l) of
--         LeftOfLine     -> Just (x, LOO)
--         RightOfLine    -> Just (x, ROO)
--         DirectlyOnLine -> Just (x, OOO)
-- XOY
classifyCut scissors l (Cut _ x _) r
  = case (sideOfScissors scissors (startPoint l), sideOfScissors scissors (endPoint r)) of
        (LeftOfLine,     LeftOfLine)     -> Just (x, LOL)
        (LeftOfLine,     RightOfLine)    -> Just (x, LOR)
        (RightOfLine,    LeftOfLine)     -> Just (x, ROL)
        (RightOfLine,    RightOfLine)    -> Just (x, ROR)
        (DirectlyOnLine, LeftOfLine)     -> Just (x, OOL)
        (DirectlyOnLine, RightOfLine)    -> Just (x, OOR)
        (RightOfLine,    DirectlyOnLine) -> Just (x, ROO)
        (LeftOfLine,     DirectlyOnLine) -> Just (x, LOO)
        (DirectlyOnLine, DirectlyOnLine) -> Just (x, OOO)

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

isSourceType, isTargetType :: Orientation -> CutType -> Bool
isSourceType Positive x = elem x [LOL, LOO, LOR, OOR, ROR]
isSourceType Negative x = isTargetType Positive x

isTargetType Positive x = elem x [LOL, OOL, ROL, ROO, ROR]
isTargetType Negative x = isSourceType Positive x
