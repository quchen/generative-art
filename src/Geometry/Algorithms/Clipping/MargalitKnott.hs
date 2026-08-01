-- | __INTERNAL MODULE__, not exposed from the package.
--
-- Binary operations on polygons (union, intersection, difference, based on a
-- fantastic paper by Margalit and Knott,
-- /An algorithm for computing the union, intersection or difference of two polygons/.
module Geometry.Algorithms.Clipping.MargalitKnott (
      IslandOrHole(..)
    , unionPP
    , intersectionPP
    , differencePP
    , antiDifferencePP
) where



import Control.Monad.State
import Data.List

import           Data.Multwomap (Multwomap)
import qualified Data.Multwomap as MM
import           Geometry.Core
import           Util (bugError)

-- $setup
-- >>> import Draw
-- >>> import Graphics.Rendering.Cairo


data Operation
    = Union
    | Intersection
    | Difference -- ^ A-B
    | AntiDifference -- ^ B-A
    deriving (Eq, Ord, Show)

-- | Type of polygons, used to distinguish results of polygon clipping.
data IslandOrHole
    = Island -- ^ An ordinary polygon. It has finite interior and infinite exterior area.
    | Hole -- ^ A hole has a finite exterior, and infinite interior.
    deriving (Eq, Ord, Show)

data RelativeOrientation = SameOrientation | OppositeOrientation deriving (Eq, Ord, Show)

data Side = Inside | Outside | Boundary deriving (Eq, Ord, Show)

-- | Let’s only deal with regular polygons for now. Or wait, are regular polygons
-- the more complicated option because they require regularization? In any case,
-- this will only become important once more pathological polygons are accepted as
-- inputs.
data Regularity = Regular deriving (Eq, Ord, Show)

-- | > Procedure 'changeOrientation' changes the orientation of the polygon Polygon.
changeOrientation :: Polygon -> Polygon
changeOrientation (Polygon ps) = Polygon (reverse ps)

-- |
-- > Table @polygonsOrientation[polygon-A-type][polygon-B-type][Oper]@ contains
-- > indicators which specify whether the two input polygons should have the same or
-- > opposite orientations according to the operation and the polygon types (table 1)
polygonsOrientation :: IslandOrHole -> IslandOrHole -> Operation -> RelativeOrientation
polygonsOrientation Island Island Intersection   = SameOrientation
polygonsOrientation Island Island Union          = SameOrientation
polygonsOrientation Island Island Difference     = OppositeOrientation
polygonsOrientation Island Island AntiDifference = OppositeOrientation

polygonsOrientation Island Hole Intersection     = OppositeOrientation
polygonsOrientation Island Hole Union            = OppositeOrientation
polygonsOrientation Island Hole Difference       = SameOrientation
polygonsOrientation Island Hole AntiDifference   = SameOrientation

polygonsOrientation Hole Island Intersection     = OppositeOrientation
polygonsOrientation Hole Island Union            = OppositeOrientation
polygonsOrientation Hole Island Difference       = SameOrientation
polygonsOrientation Hole Island AntiDifference   = SameOrientation

polygonsOrientation Hole Hole Intersection       = SameOrientation
polygonsOrientation Hole Hole Union              = SameOrientation
polygonsOrientation Hole Hole Difference         = OppositeOrientation
polygonsOrientation Hole Hole AntiDifference     = OppositeOrientation

-- |
-- > Table @fragmentype[polygon-A-type][polygon-B-type][Oper][polygon]@ contains the
-- > type of edge fragments, besides the boundary line fragments, to be selected
-- > for insertion into the line fragments table according to the operation and the
-- > polygon types (Table 2).
fragmentType :: IslandOrHole -> IslandOrHole -> Operation -> (Side, Side)
fragmentType Island Island Intersection   = (Inside, Inside)
fragmentType Island Island Union          = (Outside, Outside)
fragmentType Island Island Difference     = (Outside, Inside)
fragmentType Island Island AntiDifference = (Inside, Outside)

fragmentType Island Hole Intersection     = (Outside, Inside)
fragmentType Island Hole Union            = (Inside, Outside)
fragmentType Island Hole Difference       = (Inside, Inside)
fragmentType Island Hole AntiDifference   = (Outside, Outside)

fragmentType Hole Island Intersection     = (Inside, Outside)
fragmentType Hole Island Union            = (Outside, Inside)
fragmentType Hole Island Difference       = (Outside, Outside)
fragmentType Hole Island AntiDifference   = (Inside, Inside)

fragmentType Hole Hole Intersection       = (Outside, Outside)
fragmentType Hole Hole Union              = (Inside, Inside)
fragmentType Hole Hole Difference         = (Inside, Outside)
fragmentType Hole Hole AntiDifference     = (Outside, Inside)


data EdgeDirection
    = EdCo -- ^ In the paper: -->
    | EdAnti -- ^ In the paper: <--
    | EoAny -- ^ -- ^ In the paper: <--/-->
    deriving (Eq, Ord, Show)

--   -- | This table is a huge WTF :-E
--   --
--   -- @boundaryFragment[polygon-A-type][polygon-B-type][situation][Oper][regularity]@
--   -- contains indicators which specifies how many boundary edge fragments are to be
--   -- selected given the edge fragments situation for regular and non-regular
--   -- operations.
--   boundaryFragment
--       :: (IslandOrHole, EdgeDirection)
--       -> (IslandOrHole, EdgeDirection)
--       -> Operation
--       -> Regularity
--       -> Maybe EdgeDirection
--   -- The one sane case: agreeing edges keep agreeing. Hooray!
--   boundaryFragment (_, EdCo) (_, EdCo) _ _ = Just EdCo
--   -- Island/island regular
--   boundaryFragment (Island, EdCo)  (Island, EoAny) Union          Regular = Just EdCo
--   boundaryFragment (Island, EdCo)  (Island, EoAny) Difference     Regular = Just EdCo
--   boundaryFragment (Island, EoAny) (Island, EdCo)  Union          Regular = Just EdCo
--   boundaryFragment (Island, EoAny) (Island, EdCo)  AntiDifference Regular = Just EdCo
--   -- Island/hole, regular
--   boundaryFragment (Island, EdCo)  (Hole,   EoAny) Intersection   Regular = Just EdCo
--   boundaryFragment (Island, EdCo)  (Hole,   EoAny) AntiDifference Regular = Just EdCo
--   boundaryFragment (Island, EoAny) (Hole,   EdCo)  Union          Regular = Just EdCo
--   boundaryFragment (Island, EoAny) (Hole,   EdCo)  Difference     Regular = Just EdCo
--   -- Hole/island, regular
--   boundaryFragment (Hole,   EdCo)  (Island, EoAny) Union          Regular = Just EdCo
--   boundaryFragment (Hole,   EdCo)  (Island, EoAny) Difference     Regular = Just EdCo
--   boundaryFragment (Hole,   EoAny) (Island, EdCo)  Intersection   Regular = Just EdCo
--   boundaryFragment (Hole,   EoAny) (Island, EdCo)  Difference     Regular = Just EdCo
--   -- Hole/hole, regular
--   boundaryFragment (Hole,   EdCo)  (Hole,   EoAny) Intersection   Regular = Just EdCo
--   boundaryFragment (Hole,   EdCo)  (Hole,   EoAny) AntiDifference Regular = Just EdCo
--   boundaryFragment (Hole,   EoAny) (Hole,   EdCo)  Intersection   Regular = Just EdCo
--   boundaryFragment (Hole,   EoAny) (Hole,   EdCo)  Difference     Regular = Just EdCo
--   -- All other cases don’t yield results
--   boundaryFragment _ _ _ Regular = Nothing

-- |
-- Table @resultOrientation[polygon-A-type][polygon-B-type][Oper]@ contains
-- indicators which specify whether the type of an output result polygon is the
-- same as or the opposite of the type of polygon A when both have the same
-- orientation. If they have the opposite orientations, the orientation of the
-- result polygon is the opposite of what is written in the table.
resultOrientation :: IslandOrHole -> IslandOrHole -> Operation -> RelativeOrientation
resultOrientation Island Island Intersection   = SameOrientation
resultOrientation Island Island Union          = SameOrientation
resultOrientation Island Island Difference     = SameOrientation
resultOrientation Island Island AntiDifference = OppositeOrientation

resultOrientation Island Hole Intersection     = SameOrientation
resultOrientation Island Hole Union            = OppositeOrientation
resultOrientation Island Hole Difference       = SameOrientation
resultOrientation Island Hole AntiDifference   = SameOrientation

resultOrientation Hole Island Intersection     = OppositeOrientation
resultOrientation Hole Island Union            = SameOrientation
resultOrientation Hole Island Difference       = SameOrientation
resultOrientation Hole Island AntiDifference   = SameOrientation

resultOrientation Hole Hole Intersection       = SameOrientation
resultOrientation Hole Hole Union              = SameOrientation
resultOrientation Hole Hole Difference         = OppositeOrientation
resultOrientation Hole Hole AntiDifference     = SameOrientation

-- | Change the orientation of B so it works for the operation with A.
orientB
    :: Operation
    -> Polygon      -- ^ A
    -> Polygon      -- ^ B
    -> IslandOrHole -- ^ A’s type
    -> IslandOrHole -- ^ B’s type
    -> Polygon      -- ^ B, with possibly inverted orientation
orientB op polygonA polygonB typeA typeB =
    let orientationA = polygonOrientation polygonA
        orientationB = polygonOrientation polygonB
    in case polygonsOrientation typeA typeB op of
        SameOrientation | orientationA /= orientationB -> changeOrientation polygonB
        OppositeOrientation | orientationA == orientationB -> changeOrientation polygonB
        _otherwise -> polygonB

data CutEdge = CutEdge Vec2 [Vec2] Vec2
    deriving (Eq, Ord, Show)

cutPolygonEdges
    :: Polygon -- ^ Subject
    -> Polygon -- ^ Knives
    -> [CutEdge]
        -- ^ Subject’s edges, extended with intersection points. Note that each
        -- subject corner appears twice, once as end, and once as start of a
        -- 'CutEdge'.
cutPolygonEdges subject knives = do
    edge@(Line start end) <- polygonEdges subject
    let cuts = multiCutLine edge (polygonEdges knives)
        -- Snap each computed intersection point to the subject edge’s exact
        -- endpoints when it lies within 'snapEpsilon'. This is essential for
        -- the Margalit–Knott invariant: a vertex shared between the two
        -- polygons must be the *same* 'Vec2' (bit-identical) in both
        -- 'cutPolygon' runs, otherwise it ends up as two distinct keys in the
        -- fragment maps and 'MM.union' overflows. 'intersectionLL' computes
        -- the same geometric point via different edge pairs and can differ by
        -- a few ULPs; snapping to the endpoint eliminates that divergence.
        --
        -- Snap *before* sorting: when a shared vertex yields multiple cuts at
        -- the same geometric point, they must collapse to identical 'Vec2's
        -- before 'sortOn' so they sort adjacently; otherwise ULP-divergent
        -- copies can interleave with other cuts (e.g. a midpoint between them)
        -- and 'collapseDuplicates' (consecutive-only) fails to merge them.
        snappedCuts = map (snapToEdgeEndpoints edge) cuts
        sortedSnappedCuts = sortOn (\x -> positionAlongEdge x edge) snappedCuts
    pure (CutEdge start sortedSnappedCuts end)

-- | If a point lies within 'snapEpsilon' of either endpoint of the line,
-- return that (exact) endpoint; otherwise return the point unchanged.
snapToEdgeEndpoints :: Line -> Vec2 -> Vec2
snapToEdgeEndpoints (Line a b) p
    | normSquare (p -. a) <= snapEpsilonSquared = a
    | normSquare (p -. b) <= snapEpsilonSquared = b
    | otherwise                                  = p

-- | Tolerance for snapping computed intersection points to polygon vertices.
-- Chosen to absorb the rounding error of 'intersectionLL' (a handful of ULPs
-- on coordinates of moderate magnitude) without merging genuinely distinct
-- points. 1e-9 has been validated empirically against the regression tests;
-- shrinking it re-triggers the overflow on the floating-point shared-vertex
-- case, while growing it risks collapsing distinct close features.
snapEpsilon :: Double
snapEpsilon = 1e-9

snapEpsilonSquared :: Double
snapEpsilonSquared = snapEpsilon ^ 2

-- | Cut a line with multiple knives, and report the intersection points in order
-- along the edge.
multiCutLine :: Line -> [Line] -> [Vec2]
multiCutLine edge knives = [x | IntersectionReal x <- map (intersectionLL edge) knives]

-- Position of a point on a line relative to the line’s start in arbitrary units.
-- Useful for sorting.
positionAlongEdge :: Vec2 -> Line -> Double
positionAlongEdge p edge@(Line edgeStart _) = dotProduct (vectorOf edge) (vectorOf (Line edgeStart p))

-- | Polygon, annotated with where the points lie with respect to another polygon.
newtype CutPolygon = CutPolygon [(Side, Vec2)] deriving (Eq, Ord, Show)

cutPolygon :: Polygon -> Polygon -> CutPolygon
cutPolygon subject knives = CutPolygon (collapseDuplicates rawRing)
  where
    toVertexRing :: [CutEdge] -> [(Side, Vec2)]
    toVertexRing cutEdges =
        let go [] = []
            go (CutEdge start cuts end : rest) =
                (pointInPolygonOrBoundary start knives, start)
                :  [(Boundary, p) | p <- cuts, p /= end, p /= start]
                ++ go rest
                               -- ^^^^^^^^^^^^^^^^
                               -- Drop cut points that coincide with the edge's
                               -- *start* too: the start is already emitted
                               -- explicitly above, so a knife edge that passes
                               -- through the start vertex would otherwise
                               -- duplicate it. The duplicate is non-consecutive
                               -- (interleaved with other cuts along the edge),
                               -- so 'collapseDuplicates' (consecutive-only)
                               -- cannot merge it; this corrupts the vertex ring
                               -- and overflows 'Multwomap'.
                               --
                               -- end will be handled as the start of the next cut.
                               -- A cut point that coincides with this edge's end is
                               -- dropped here so it isn't double-counted.
        in go cutEdges

    rawRing = toVertexRing (cutPolygonEdges subject knives)

    -- Collapse consecutive duplicate points in the vertex ring. This is the
    -- key to keeping the 'Multwomap' invariant (at most two fragments per
    -- vertex: one in, one out) intact. Duplicates arise when a cut point
    -- coincides with a subject vertex. Thanks to 'snapToEdgeEndpoints', such
    -- a cut is now bit-identical to the vertex, so a plain '==' suffices to
    -- spot the duplicate. Two situations:
    --
    --   1. A cut point coincides with this edge's /start/ (a knife edge passes
    --      through the subject vertex at the start of the edge). The vertex
    --      then appears once as @(pointInPolygonOrBoundary start, start)@ and
    --      once as @(Boundary, cut)@.
    --
    --   2. Two knife edges share a vertex that lies on a subject edge. Both
    --      knife edges report the same intersection point, so the cut list
    --      contains the same point twice.
    --
    -- Without collapsing, the duplicate coordinate becomes a key with three
    -- or more outgoing fragments after 'MM.union', crashing 'Multwomap'.
    --
    -- When two consecutive entries share a point, 'Boundary' wins over
    -- 'Inside'/'Outside' — a point that is an intersection (always 'Boundary')
    -- is on the other polygon's outline, and 'Boundary' is the truthful
    -- classification for fragment selection.
    collapseDuplicates :: [(Side, Vec2)] -> [(Side, Vec2)]
    collapseDuplicates [] = []
    collapseDuplicates (x : xs) =
        let (kept, rest) = span (samePoint x) xs
            winningSide  = foldl' combineSide (fst x) (map fst kept)
        in (winningSide, snd x) : collapseDuplicates rest

    samePoint (_, p1) (_, p2) = p1 == p2
    combineSide Boundary _ = Boundary
    combineSide _ Boundary = Boundary
    combineSide s _        = s

pointInPolygonOrBoundary :: Vec2 -> Polygon -> Side
pointInPolygonOrBoundary p polygon
    | pointOnPolygonBoundary p polygon = Boundary
    | pointInPolygon p polygon         = Inside
    | otherwise                        = Outside

-- | Exact predicate: is the point on any edge of the polygon (including the
-- endpoints)? Uses exact arithmetic on 'Double' coordinates — a point lies on
-- a segment iff it is collinear with the segment's endpoints (cross product
-- is exactly 0) and its projection onto the segment lies within the segment
-- (dot products at both ends are non-negative). No epsilon, so coincident
-- intersection points and shared vertices are detected deterministically.
pointOnPolygonBoundary :: Vec2 -> Polygon -> Bool
pointOnPolygonBoundary p polygon = any onEdge (polygonEdges polygon)
  where
    onEdge (Line a b) =
        let ab = b -. a
            ap' = p -. a
            crossProduct = cross ab ap'
            -- Collinear (exact). Guard against the degenerate zero-length edge.
            isCollinear = crossProduct == 0
            -- p's projection lies within [a,b]: dot(ap,ab) >= 0 && dot(pb,ab) >= 0
            bp = p -. b
            withinSegment = dotProduct ap' ab >= 0 && dotProduct bp ab >= 0
        in isCollinear && withinSegment

buildEdgeFragementMap :: CutPolygon -> Side -> Polygon -> Either String (Multwomap Vec2 Vec2)
buildEdgeFragementMap (CutPolygon vr) ty polygonOther =
    let insertEdgeFragement :: (Side, Vec2) -> (Side, Vec2) -> Multwomap Vec2 Vec2 -> Either String (Multwomap Vec2 Vec2)
        insertEdgeFragement (Boundary, x) (Boundary, y) = case pointInPolygonOrBoundary ((x +. y) /. 2) polygonOther of
            Boundary -> MM.insert x y
            inOrOut | inOrOut == ty -> MM.insert x y
            _otherwise -> Right
        insertEdgeFragement (pointSideX, x) (pointSideY, y)
            | ty == pointSideX || ty == pointSideY = MM.insert x y
        insertEdgeFragement _other _wise = Right

        inserts = zipWith insertEdgeFragement vr (tail (cycle vr))
    in foldl' (\mmap f -> mmap >>= f) (Right MM.empty) inserts

constructResultPolygons :: Multwomap Vec2 Vec2 -> [Polygon]
constructResultPolygons mmap = evalState reconstructAllS mmap

reconstructAllS :: State (Multwomap Vec2 Vec2) [Polygon]
reconstructAllS = gets MM.arbitraryKey >>= \case
    Nothing -> pure []
    Just start -> do
        polygons <- reconstructLoopsFromS start
        rest <- reconstructAllS
        pure (polygons ++ rest)

-- | Reconstruct all simple sub-polygons that pass through @start@.
--
-- The fragment map guarantees at most two fragments per vertex (one in, one
-- out). Nevertheless, the walk can produce a /self-touching/ (weakly-simple)
-- polygon: when @start@ has two outgoing fragments, the walk revisits @start@
-- mid-traversal, producing a figure-eight (e.g. @[X, Y, X, Z]@). Such a
-- polygon violates the simple-polygon assumption of downstream consumers and
-- re-feeding it into 'margalitKnott' overflows 'Multwomap'.
--
-- We split at every return to @start@: each closed loop becomes its own
-- simple polygon. Degenerate loops (fewer than 3 vertices, i.e. a back-and-for
-- sliver) are discarded.
reconstructLoopsFromS :: Vec2 -> State (Multwomap Vec2 Vec2) [Polygon]
reconstructLoopsFromS start = go [start]
  where
    go :: [Vec2] -> State (Multwomap Vec2 Vec2) [Polygon]
    go path = gets (MM.extract (last path)) >>= \case
        Nothing -> pure (emit path)
        Just (next, rest) -> do
            put rest
            if next == start
                then -- Loop closed: `path` is a complete simple polygon.
                     -- If `start` still has a fragment, a new loop begins.
                    gets (MM.extract start) >>= \case
                        Nothing -> pure (emit path)
                        Just (next2, rest2) -> do
                            put rest2
                            polys <- go [start, next2]
                            pure (emit path ++ polys)
                else go (path ++ [next])
    -- Keep only non-degenerate polygons (≥ 3 vertices); a 2-vertex loop is a
    -- zero-area sliver (edge traversed forward then back) that carries no area.
    emit xs = [Polygon xs | length xs >= 3]

-- | The paper’s code on this is pretty unclear, if not misleading: it talks about
-- a »current polygon« and a »last result polygon«. By testing, it turns out those
-- two are the same, and one should simply look at one polygon, and not two
-- neighbours.
addTypes :: Operation -> PolygonOrientation -> IslandOrHole -> IslandOrHole -> [Polygon] -> [(Polygon, IslandOrHole)]
addTypes op orientationA polygonA_Type polygonB_Type = go
  where
    go [] = []
    go (currentPolygon:rest) =
        let orientationsMatch = polygonOrientation currentPolygon == orientationA
            orientationsShouldMatch = resultOrientation polygonA_Type polygonB_Type op == SameOrientation
            orient
                | orientationsMatch == orientationsShouldMatch = id
                | otherwise = flipHoleIsland
        in (currentPolygon, orient polygonA_Type) : go rest

    flipHoleIsland Island = Hole
    flipHoleIsland Hole = Island

margalitKnott :: Operation -> Regularity -> (Polygon, IslandOrHole) -> (Polygon, IslandOrHole) -> [(Polygon, IslandOrHole)]
margalitKnott op Regular (polygonA', polygonA_Type) (polygonB', polygonB_Type) =
    let polygonA = sanitizePolygon polygonA'
        polygonB = orientB op polygonA (sanitizePolygon polygonB') polygonA_Type polygonB_Type

        vertexRingA = cutPolygon polygonA polygonB
        vertexRingB = cutPolygon polygonB polygonA

        (ftA, ftB) = fragmentType polygonA_Type polygonB_Type op
    in case ( buildEdgeFragementMap vertexRingA ftA polygonB
            , buildEdgeFragementMap vertexRingB ftB polygonA
            ) of
        (Left err, _) -> reportOverflow err polygonA polygonB
        (_, Left err) -> reportOverflow err polygonA polygonB
        (Right efA, Right efB) -> case MM.union efA efB of
            Left err          -> reportOverflow err polygonA polygonB
            Right edgeFragments ->
                let polygons = constructResultPolygons edgeFragments
                    polygonsTyped = addTypes op (polygonOrientation polygonA) polygonA_Type polygonB_Type polygons
                    -- TODO: boundary edge fragment handling
                in polygonsTyped
  where
    -- An overflow in 'Multwomap' means a vertex has three or more distinct
    -- outgoing fragments, which violates the Margalit–Knott invariant. This
    -- is a bug in fragment generation, not user input. Instead of crashing
    -- opaquely, report the two input polygons that triggered it so the bug
    -- can be reproduced and diagnosed.
    reportOverflow :: String -> Polygon -> Polygon -> a
    reportOverflow err polygonA polygonB =
        bugError "MargalitKnott" $ unlines
            [ err
            , "Polygon A: " ++ show polygonA
            , "Polygon B: " ++ show polygonB
            ]

-- | Drop consecutive duplicate vertices (and a duplicate last vertex that
-- repeats the first) from a polygon's corner list. The Margalit–Knott
-- algorithm assumes a /simple/ polygon ring: 'polygonEdges' closes the ring
-- implicitly via @tail (cycle ps)@, so a repeated first/last vertex produces
-- a zero-length edge, which in turn spawns a self-loop fragment
-- (@x -> x@) and overflows 'Multwomap'. Likewise, any run of equal
-- consecutive vertices collapses to a single point.
sanitizePolygon :: Polygon -> Polygon
sanitizePolygon (Polygon []) = Polygon []
sanitizePolygon (Polygon ps) =
    Polygon (dropTrailingDuplicate (foldr cons [] ps))
  where
    -- Build the deduplicated list back-to-front, skipping a vertex equal to
    -- the one we just prepended.
    cons x acc
        | (y : _) <- acc, x == y = acc
        | otherwise              = x : acc
    -- If, after dedup, the last vertex equals the first, drop it: the ring is
    -- closed implicitly by 'polygonEdges'.
    dropTrailingDuplicate xs
        | (x:ys) <- xs, not (null ys), last ys == x = init ys
        | otherwise                                 = xs

-- | Split a /self-touching/ (weakly-simple) polygon into a list of simple
-- polygons. The Margalit–Knott algorithm assumes simple inputs: each vertex
-- has at most two incident edges (one in, one out). A self-touching polygon
-- pinches at one or more vertices that it visits twice non-adjacently, giving
-- that vertex four incident edges and overflowing 'Multwomap'.
--
-- Self-touching polygons arise as output of 'constructResultPolygons' when the
-- fragment graph forms a figure-eight (two loops sharing a vertex); they must
-- be split before they are fed back into 'margalitKnott'.
--
-- Algorithm: walk the closed vertex ring. When the next vertex is already
-- present earlier in the current walk, the segment between the two occurrences
-- forms a closed simple sub-polygon; emit it, then resume the outer walk from
-- the first occurrence (dropping the just-emitted loop). This handles nested
-- and chained pinches. After splitting, each result is re-sanitized (to drop
-- any zero-length slivers) and degenerate results (< 3 vertices) are dropped.
--
-- A simple polygon (no self-touch) is returned as a singleton list unchanged.
splitSelfTouchingPolygon :: Polygon -> [Polygon]
splitSelfTouchingPolygon (Polygon ps0) =
    filter hasMinThree (map (sanitizePolygon . Polygon) loops)
  where
    hasMinThree (Polygon xs) = length xs >= 3
    -- The vertex ring is closed implicitly, so we do not append the first
    -- vertex at the end; instead, a loop closes when we revisit a vertex
    -- already in the walk.
    loops = go ps0 []
    -- walk: remaining vertices to consume; current path of visited vertices.
    go [] path = finalize path
    go (x:xs) path
        | Just i <- lookup x (zip path [0..]) =
            -- x already appears at position i in path: extract the sub-loop
            -- [path[i..end] ++ x], then resume with path[0..i] ++ xs.
            let loop = drop i path ++ [x]
                outer = take i path
            in loop : go xs outer
        | otherwise = go xs (path ++ [x])
    -- When the input is exhausted, the residual path is the last loop if
    -- non-empty; otherwise it was already fully consumed.
    finalize [] = []
    finalize path = [path]

-- | Union of two polygons.
--
-- <<docs/haddock/Geometry/Algorithms/Clipping/MargalitKnott/union.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping/MargalitKnott/union.svg" 150 150 $ \_ -> do
--     let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 100 100]
--         p2 = boundingBoxPolygon [Vec2 50 50, Vec2 140 140]
--     for_ (unionPP p1 p2) $ \(polygon, _ty) -> cairoScope $ do
--         sketch polygon
--         setColor (mma 1 `withOpacity` 0.2)
--         fill
--     sketch (p1, p2) >> stroke
-- :}
-- Generated file: size 2KB, crc32: 0xcc4c9f5e
unionPP :: Polygon -> Polygon -> [(Polygon, IslandOrHole)]
unionPP = ppBinop Union

-- | Intersection of two polygons.
--
-- The union will always be 'Island's, but for homogenity of types with
-- 'intersectionPP' etc. the type is included anyway.
--
-- <<docs/haddock/Geometry/Algorithms/Clipping/MargalitKnott/intersection.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping/MargalitKnott/intersection.svg" 150 150 $ \_ -> do
--     let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 100 100]
--         p2 = boundingBoxPolygon [Vec2 50 50, Vec2 140 140]
--     for_ (intersectionPP p1 p2) $ \(polygon, _ty) -> cairoScope $ do
--         sketch polygon
--         setColor (mma 1 `withOpacity` 0.2)
--         fill
--     sketch (p1, p2) >> stroke
-- :}
-- Generated file: size 2KB, crc32: 0xdaf13db5
intersectionPP :: Polygon -> Polygon -> [(Polygon, IslandOrHole)]
intersectionPP = ppBinop Intersection

-- | Difference of two polygons: anything that is in the first argument, but not in the second.
--
-- <<docs/haddock/Geometry/Algorithms/Clipping/MargalitKnott/difference.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping/MargalitKnott/difference.svg" 150 150 $ \_ -> do
--     let p1 = boundingBoxPolygon [Vec2 10 10, Vec2 100 100]
--         p2 = boundingBoxPolygon [Vec2 50 50, Vec2 140 140]
--     for_ (differencePP p1 p2) $ \(polygon, _ty) -> cairoScope $ do
--         sketch polygon
--         setColor (mma 1 `withOpacity` 0.2)
--         fill
--     sketch (p1, p2) >> stroke
-- :}
-- Generated file: size 2KB, crc32: 0x9388b325
differencePP
    :: Polygon -- ^ A
    -> Polygon -- ^ B
    -> [(Polygon, IslandOrHole)] -- ^ A-B
differencePP = ppBinop Difference

-- | Anti-difference of two polygons: anything that is in the second argument, but
-- not in the first. Results are identical to @'flip' 'differencePP'@.
antiDifferencePP
    :: Polygon -- ^ A
    -> Polygon -- ^ B
    -> [(Polygon, IslandOrHole)] -- ^ B-A
antiDifferencePP = ppBinop AntiDifference

ppBinop :: Operation -> Polygon -> Polygon -> [(Polygon, IslandOrHole)]
ppBinop op p1 p2 =
    let as = map (\a -> (a, Island)) (splitSelfTouchingPolygon p1)
        bs = map (\b -> (b, Island)) (splitSelfTouchingPolygon p2)
    in concatMap (\a -> concatMap (margalitKnott op Regular a) bs) as
