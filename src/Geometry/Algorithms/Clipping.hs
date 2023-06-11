-- | Functions to cut things into pieces.
--
-- <<docs/haddock/Geometry/Algorithms/Clipping/complicated_intersection.svg>>
--
-- === __(image code)__
-- >>> import Draw
-- >>> import qualified Graphics.Rendering.Cairo as C
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping/complicated_intersection.svg" 200 160 $ do
--     let p1 = Polygon
--             [ Vec2 40 30, Vec2 140 30, Vec2 140 140, Vec2 120 140, Vec2 120 80, Vec2 100 80
--             , Vec2 100 140, Vec2 80 140, Vec2 80 60, Vec2 60 60, Vec2 60 140, Vec2 40 140 ]
--         p2 = Polygon
--             [ Vec2 180 20, Vec2 130 20, Vec2 130 35, Vec2 120 35, Vec2 120 20, Vec2 110 20
--             , Vec2 110 35, Vec2 100 35, Vec2 100 20, Vec2 90 20, Vec2 90 35, Vec2 80 35
--             , Vec2 80 20, Vec2 70 20, Vec2 70 35, Vec2 60 35, Vec2 60 20, Vec2 20 20
--             , Vec2 20 40, Vec2 170 60, Vec2 50 80, Vec2 170 100, Vec2 10 120, Vec2 180 140 ]
--         cutResult = intersectionPP p1 p2
--     setLineJoin LineJoinRound
--     cairoScope $ do
--         setLineWidth 1
--         setColor (mma 0)
--         sketch p1
--         stroke
--     cairoScope $ do
--         setLineWidth 1
--         setColor (mma 1)
--         sketch p2
--         stroke
--     for_ (zip [2..] cutResult) $ \(i, (polygon, _islandOrHole)) -> cairoScope $ do
--         setLineWidth 2
--         sketch polygon
--         setColor (mma i `withOpacity` 0.2)
--         fillPreserve
--         setColor (mma i)
--         stroke
-- :}
-- Generated file: size 5KB, crc32: 0xd81cbd60
module Geometry.Algorithms.Clipping (
      cutLineWithLine
    , CutLine(..)
    , cutPolygon
    , cohenSutherland

    , LineType(..)
    , clipPolygonWithLine
    , clipPolygonWithLineSegment

    , sutherlandHodgman
    , IslandOrHole(..)
    , intersectionPP
    , unionPP
    , differencePP

    , hatch
) where



import Geometry.Algorithms.Clipping.CohenSutherland
import Geometry.Algorithms.Clipping.Internal
import Geometry.Algorithms.Clipping.MargalitKnott
    (IslandOrHole (..), differencePP, intersectionPP, unionPP)
import Geometry.Algorithms.Clipping.SutherlandHodgman
import Geometry.Core

-- $setup
-- >>> import Draw
-- >>> import Graphics.Rendering.Cairo

-- | Add shading lines to a polygon. This is especially useful for a pen plotter to
-- do shading, hence the name.
--
-- <<docs/haddock/Geometry/Algorithms/Clipping/hatched_polygon.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping/hatched_polygon.svg" 100 100 $ do
--     let polygon = Polygon [Vec2 10 10, Vec2 70 45, Vec2 90 10, Vec2 90 90, Vec2 50 55, Vec2 10 90]
--     let hatching = hatch polygon (deg 30) 10 0
--     cairoScope $ do
--         for_ hatching sketch
--         setColor (mma 1)
--         stroke
--     sketch polygon
--     stroke
-- :}
-- Generated file: size 2KB, crc32: 0x81dce6d7
hatch
    :: Polygon
    -> Angle -- ^ Direction in which the lines will point. @'deg' 0@ is parallel to the x axis.
    -> Double -- ^ Distance between shading lines
    -> Double -- ^ An offset of 0 means a line will go through the center of the polygon's 'BoundingBox'.
    -> [Line]
hatch polygon angle interval offset = do
    let transformation = rotate (negateV angle)
        polygonAligned = transform transformation polygon
        BoundingBox (Vec2 xLo yLo) (Vec2 xHi yHi) = boundingBox polygonAligned
        yMid = (yLo + yHi) / 2 + offset
        yUp = takeWhile (< yHi) (iterate (+ interval) yMid)
        yDown = takeWhile (> yLo) (tail (iterate (subtract interval) yMid))
    horizontalScissors <- do
            y <- yUp ++ yDown
            pure (Line (Vec2 xLo y) (Vec2 xHi y))
    horizontalHatches <- [line | (line, LineInsidePolygon) <- clipPolygonWithLine polygonAligned horizontalScissors]
    pure (transform (inverse transformation) horizontalHatches)
