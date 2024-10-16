module Geometry.Algorithms.Clipping.SutherlandHodgman (sutherlandHodgman) where



import Geometry.Core


-- $setup
-- >>> import Draw
-- >>> import Graphics.Rendering.Cairo

-- | Clip a polygon with a convex clipping mask using the
-- [Sutherland-Hodgman](https://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman_algorithm)
-- algorithm.
--
-- __Note:__ The precondition (convexity) is not checked!
--
-- <<docs/haddock/Geometry/Algorithms/Clipping/SutherlandHodgman/sutherland_hodgman.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping/SutherlandHodgman/sutherland_hodgman.svg" 110 100 $ \_ -> do
--     let subject = Polygon [Vec2 10 10, Vec2 10 90, Vec2 50 50, Vec2 90 90, Vec2 90 10]
--         Polygon scissors' = boundingBoxPolygon [Vec2 40 30, Vec2 100 80]
--         scissors = Polygon ( scissors')
--         clipped = sutherlandHodgman subject scissors
--     sketch clipped >> setColor (mma 0 `withOpacity` 0.5) >> fill
--     sketch subject >> setColor (mma 1) >> stroke
--     sketch scissors >> setColor (mma 3) >> setDash [3,3] 0 >> stroke
-- :}
-- Generated file: size 2KB, crc32: 0x722cb97a
sutherlandHodgman
    :: Polygon -- ^ Subject
    -> Polygon -- ^ __Convex__ scissors
    -> Polygon
sutherlandHodgman subject scissors = foldl (cutOffEdge (polygonOrientation scissors)) subject (polygonEdges scissors)

cutOffEdge :: PolygonOrientation -> Polygon -> Line -> Polygon
cutOffEdge o (Polygon corners) clipEdge = Polygon . concat $ zipWith
    (\p1 p2 ->
        let intersectingPoint = intersectInfiniteLines (Line p1 p2) clipEdge
        in if inside o p2 clipEdge
            then if outside o p1 clipEdge
                then [intersectingPoint, p2]
                else [p2]
            else if inside o p1 clipEdge
                then [intersectingPoint]
                else []
        )
    corners
    (tail (cycle corners))

inside :: PolygonOrientation -> Vec2 -> Line -> Bool
inside o p (Line a b) = cross (a -. p) (b -. p) `f` 0
  where
    f = case o of
        PolygonPositive -> (>) -- Sign determined by testing :-E
        PolygonNegative -> (<)

outside :: PolygonOrientation -> Vec2 -> Line -> Bool
outside o p = not . inside o p
