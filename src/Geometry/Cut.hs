module Geometry.Cut (
      cutLine
    , CutLine(..)
    , cutPolygon
) where



import           Data.List
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Set      as S

import Geometry.Core



-- | Used in the implementation of a multimap where each entry can have one or
-- two values.
data OneOrTwo a = One a | Two a a
    deriving (Eq, Ord, Show)

-- | Cut a polygon in multiple pieces with a line.
--
-- For convex polygons, the result is either just the polygon (if the line
-- misses) or two pieces. Concave polygons can in general be divided in
-- arbitrarily many pieces.
cutPolygon :: Line -> Polygon -> [Polygon]
cutPolygon scissors polygon =
    reconstructPolygons
        (edgeMap scissors
            (cutAll scissors
                (polygonEdges polygon)))

-- Generate a list of all the edges of a polygon, extended with additional
-- points on the edges that are crossed by the scissors.
cutAll :: Line -> [Line] -> [CutLine]
cutAll scissors edges = map (cutLine scissors) edges

edgeMap :: Line -> [CutLine] -> Map Vec2 (OneOrTwo Vec2)
edgeMap scissors allCuts = (newCutsEdgeMapBuilder scissors allCuts . polygonEdgeMapBuilder allCuts) M.empty

newCutsEdgeMapBuilder :: Line -> [CutLine] -> Map Vec2 (OneOrTwo Vec2) -> Map Vec2 (OneOrTwo Vec2)
newCutsEdgeMapBuilder scissors@(Line scissorsStart _) cuts = go cutPointsSorted
  where
    go ((p, pTy) : (q, qTy) : rest)
        | isSourceType pTy && isTargetType qTy = (p --> q) . (q --> p) . go rest
    go (_:_) = bugError "Unpaired cut point"
    go [] = id

    cutPointsSorted :: [(Vec2, CutType)]
    cutPointsSorted = sortOn (scissorCoordinate . fst) (M.toList recordedNeighbours)

    recordedNeighbours :: Map Vec2 CutType
    recordedNeighbours
      = M.fromList (catMaybes (zipWith3 (recordNeighbours scissors)
                                        cuts
                                        (tail (cycle cuts))
                                        (tail (tail (cycle cuts)))))

    -- How far ahead/behind the start of the line is the point?
    --
    -- In mathematical terms, this yields the coordinate of a point in the
    -- 1-dimensional vector space that is the scissors line.
    scissorCoordinate :: Vec2 -> Double
    scissorCoordinate p = dotProduct (vectorOf scissors) (vectorOf (Line scissorsStart p))

-- A polygon can be described by an adjacency list of corners to the next
-- corner. A cut simply introduces two new corners (of polygons to be) that
-- point to each other.
polygonEdgeMapBuilder :: [CutLine] -> Map Vec2 (OneOrTwo Vec2) -> Map Vec2 (OneOrTwo Vec2)
polygonEdgeMapBuilder cuts = case cuts of
    Cut p x q : rest -> (p --> x) . (x --> q) . polygonEdgeMapBuilder rest
    NoCut p q : rest -> (p --> q) . polygonEdgeMapBuilder rest
    []               -> id

-- Insert a value into a (1 to 2) multimap. Self-references are not allowed.
(-->) :: Ord a => a -> a -> Map a (OneOrTwo a) -> Map a (OneOrTwo a)
(k --> v) db = case M.lookup k db of
    _ | k == v    -> db
    Nothing       -> M.insert k (One v) db
    Just (One v') -> M.insert k (Two v v') db
    Just Two{}    -> bugError "Third edge in cutting algorithm"

-- Given a list of corners that point to other corners, we can reconstruct
-- all the polygons described by them by finding the smallest cycles, i.e.
-- cycles that do not contain other (parts of the) adjacency map.
--
-- Starting at an arbitrary point, we can extract a single polygon by
-- following such a minimal cycle; iterating this algorithm until the entire
-- map has been consumed yields all the polygons.
reconstructPolygons :: Map Vec2 (OneOrTwo Vec2) -> [Polygon]
reconstructPolygons = \edgeGraph -> case M.lookupMin edgeGraph of
    Nothing -> []
    Just (edgeStart, _end) ->
        let (poly, edgeGraph') = extractSinglePolygon Nothing S.empty edgeStart edgeGraph
        in poly : reconstructPolygons edgeGraph'
  where
    extractSinglePolygon lastPivot visited pivot edgeGraph
      = case M.lookup pivot edgeGraph of
            _ | S.member pivot visited -> (Polygon [], edgeGraph)
            Nothing -> (Polygon [], edgeGraph)
            Just (One next) ->
                let (Polygon rest, edgeGraph') = extractSinglePolygon
                        (Just pivot)
                        (S.insert pivot visited)
                        next
                        (M.delete pivot edgeGraph)
                in (Polygon (pivot:rest), edgeGraph')
            Just (Two next1 next2) ->
                let endAtSmallestAngle = case lastPivot of
                        Nothing -> next1 -- arbitrary starting point WLOG
                        Just from -> let forwardness end = dotProduct (direction (Line from pivot))
                                                                      (direction (Line pivot end))
                                     in minimumBy (comparing forwardness) (filter (/= from) [next1, next2])
                    unusedNext = if endAtSmallestAngle == next1 then next2 else next1
                    (Polygon rest, edgeGraph') = extractSinglePolygon
                        (Just pivot)
                        (S.insert pivot visited)
                        endAtSmallestAngle
                        (M.insert pivot (One unusedNext) edgeGraph)
                in (Polygon (pivot:rest), edgeGraph')

recordNeighbours :: Line -> CutLine -> CutLine -> CutLine -> Maybe (Vec2, CutType)
recordNeighbours scissors cutL (Cut pM xM qM) cutR
    | pM == xM = case cutL of
        NoCut  pL    _qLpM -> Just (xM, classifyCut scissors ((False, pL), (False, qM)))
        Cut   _pL xL _qLpM -> Just (xM, classifyCut scissors ((True,  xL), (False, qM)))
    | xM == qM = case cutR of
        NoCut _qMpR     qR -> Just (xM, classifyCut scissors ((False, pM), (False, qR)))
        Cut   _qMpR xR _qR -> Just (xM, classifyCut scissors ((False, pM), (True,  xR)))
recordNeighbours _ _ _ _ = Nothing

-- ( (<is left  on scissors?>, <left  neighbour>)
-- , (<is right on scissors?>, <right neighbour>)
-- )
classifyCut :: Line -> ((Bool, Vec2), (Bool, Vec2)) -> CutType
classifyCut scissors ((False, p), (False, q)) = case (sideOfScissors scissors p, sideOfScissors scissors q) of
    (LeftOfLine,  LeftOfLine)  -> LOL
    (LeftOfLine,  RightOfLine) -> LOR
    (RightOfLine, LeftOfLine)  -> ROL
    (RightOfLine, RightOfLine) -> ROR
    _other -> error "Point on scissors that is has not been recorded as such! [XOY type]"
classifyCut scissors ((False, p), (True,  _)) = case sideOfScissors scissors p of
    LeftOfLine  -> LOO
    RightOfLine -> ROO
    _other -> error "Point on scissors that is has not been recorded as such! [XOO type]"
classifyCut scissors ((True, _), (False, q)) = case sideOfScissors scissors q of
    LeftOfLine  -> OOL
    RightOfLine -> OOR
    _other -> error "Point on scissors that is has not been recorded as such! [OOX type]"
classifyCut _scissors ((True, _), (True,  _)) = OOO

sideOfScissors :: Line -> Vec2 -> SideOfLine
sideOfScissors scissors@(Line scissorsStart _) p
  = let scissorsCrossPoint = det (vectorOf scissors) (vectorOf (Line scissorsStart p))
    in case compare 0 scissorsCrossPoint of
        LT -> LeftOfLine
        EQ -> DirectlyOnLine
        GT -> RightOfLine

data CutLine
    = NoCut Vec2 Vec2
        -- ^ (start, end). No cut has occurred, i.e. the cutting line did not
        -- intersect with the object.
    | Cut Vec2 Vec2 Vec2
        -- ^ (start, cut, end). The input was divided in two lines.
    deriving (Eq, Ord, Show)

data SideOfLine = LeftOfLine | DirectlyOnLine | RightOfLine
    deriving (Eq, Ord, Show)

data CutType
    = LOL
        -- ^
        -- @
        --    p     q
        --     \   /
        --      \ /
        -- ===== x =====>
        -- @

    | LOO
        -- ^
        -- @
        --       p
        --       |
        --       |
        -- ===== x ----- q =====>
        -- @

    | LOR
        -- ^
        -- @
        --       p
        --       |
        --       |
        -- ===== x =====>
        --       |
        --       |
        --       q
        -- @

    | OOL
        -- ^
        -- @
        --               q
        --               |
        --               |
        -- ===== p ----- x =====>
        -- @

    | OOO
        -- ^
        -- @
        -- ===== p ----- x ----- q =====>
        -- @

    | OOR
        -- ^
        -- @
        -- ===== p ----- x =====>
        --               |
        --               |
        --               q
        -- @

    | ROL
        -- ^
        -- @
        --       q
        --       |
        --       |
        -- ===== x =====>
        --       |
        --       |
        --       p
        -- @

    | ROO
        -- ^
        -- @
        -- ===== x ----- q =====>
        --       |
        --       |
        --       p
        -- @

    | ROR
        -- ^
        -- @
        -- ===== x =====>
        --      / \
        --     /   \
        --    p     q
        -- @
    deriving (Eq, Ord, Show)

isSourceType, isTargetType :: CutType -> Bool
isSourceType x = elem x [LOL, LOO, LOR, OOR, ROR]
isTargetType x = elem x [LOL, OOL, ROL, ROO, ROR]

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
