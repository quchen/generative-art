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
    let cuts = sortOn (\x -> positionAlongEdge x edge) (multiCutLine edge (polygonEdges knives))
    pure (CutEdge start cuts end)

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
cutPolygon subject knives = toVertexRing (cutPolygonEdges subject knives)
  where
    toVertexRing :: [CutEdge] -> CutPolygon
    toVertexRing cutEdges =
        let go [] = []
            go (CutEdge start cuts _end : rest) = (pointInPolygonOrBoundary start knives, start) : [(Boundary, p) | p <- cuts] ++ go rest
                               --  ^^^^ end will be handled as the start of the next cut
        in CutPolygon (go cutEdges)

pointInPolygonOrBoundary :: Vec2 -> Polygon -> Side
pointInPolygonOrBoundary p polygon = if pointInPolygon p polygon
    then Inside
    else Outside -- TODO: implement point-is-on-boundary!

buildEdgeFragementMap :: CutPolygon -> Side -> Polygon -> Multwomap Vec2 Vec2
buildEdgeFragementMap (CutPolygon vr) ty polygonOther =
    let insertEdgeFragement :: (Side, Vec2) -> (Side, Vec2) -> Multwomap Vec2 Vec2 -> Multwomap Vec2 Vec2
        insertEdgeFragement (Boundary, x) (Boundary, y) = case pointInPolygonOrBoundary ((x +. y) /. 2) polygonOther of
            Boundary -> MM.insert x y
            inOrOut | inOrOut == ty -> MM.insert x y
            _otherwise -> id
        insertEdgeFragement (pointSideX, x) (pointSideY, y)
            | ty == pointSideX || ty == pointSideY = MM.insert x y
        insertEdgeFragement _other _wise = id

        inserts = zipWith insertEdgeFragement vr (tail (cycle vr))
    in foldl' (\mmap f -> f mmap) MM.empty inserts

constructResultPolygons :: Multwomap Vec2 Vec2 -> [Polygon]
constructResultPolygons mmap = evalState reconstructAllS mmap

reconstructAllS :: State (Multwomap Vec2 Vec2) [Polygon]
reconstructAllS = gets MM.arbitraryKey >>= \case
    Nothing -> pure []
    Just start -> do
        polygon <- reconstructSingleS start
        rest <- reconstructAllS
        pure (polygon : rest)

reconstructSingleS :: Vec2 -> State (Multwomap Vec2 Vec2) Polygon
reconstructSingleS start = gets (MM.extract start) >>= \case
    Nothing -> pure (Polygon [])
    Just (next, restEdgeFragments) -> do
        put restEdgeFragments
        Polygon rest <- reconstructSingleS next
        pure (Polygon (start : rest))

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
margalitKnott op Regular (polygonA, polygonA_Type) (polygonB', polygonB_Type) =
    let polygonB = orientB op polygonA polygonB' polygonA_Type polygonB_Type

        vertexRingA = cutPolygon polygonA polygonB
        vertexRingB = cutPolygon polygonB polygonA

        (ftA, ftB) = fragmentType polygonA_Type polygonB_Type op
        efA = buildEdgeFragementMap vertexRingA ftA polygonB
        efB = buildEdgeFragementMap vertexRingB ftB polygonA
        edgeFragments = MM.union efA efB

        polygons = constructResultPolygons edgeFragments

        polygonsTyped = addTypes op (polygonOrientation polygonA) polygonA_Type polygonB_Type polygons

        -- TODO: boundary edge fragment handling

    in polygonsTyped

-- | Union of two polygons.
--
-- <<docs/haddock/Geometry/Algorithms/Clipping/MargalitKnott/union.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping/MargalitKnott/union.svg" 150 150 $ do
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
-- haddockRender "Geometry/Algorithms/Clipping/MargalitKnott/intersection.svg" 150 150 $ do
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
-- haddockRender "Geometry/Algorithms/Clipping/MargalitKnott/difference.svg" 150 150 $ do
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
ppBinop op p1 p2 = margalitKnott op Regular (p1, Island) (p2, Island)
