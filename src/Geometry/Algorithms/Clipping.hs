module Geometry.Algorithms.Clipping (
      cutLineWithLine
    , CutLine(..)
    , cutPolygon

    , LineType(..)
    , clipPolygonWithLine
    , clipPolygonWithLineSegment

    , IslandOrHole(..)
    , intersectionPP
    , unionPP
    , differencePP

    , hatch
) where



import Geometry.Algorithms.Clipping.Internal
import Geometry.Algorithms.Clipping.MargalitKnott
    (IslandOrHole (..), differencePP, intersectionPP, unionPP)
import Geometry.Core

-- $setup
-- >>> import Draw
-- >>> import Graphics.Rendering.Cairo

-- | Add shading lines to a polygon. This is especially useful for a pen plotter to
-- do shading, hence the name.
--
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping.hs/hatched_polygon.svg" 100 100 $ do
--     let polygon = Polygon [Vec2 10 10, Vec2 90 10, Vec2 90 90, Vec2 10 90]
--     let shading = hatch polygon (deg 30) 10
--     sketch polygon
--     stroke
--     for_ shading sketch
--     setColor (mathematica97 1)
--     stroke
-- :}
-- docs/haddock/Geometry/Algorithms/Clipping.hs/hatched_polygon.svg
--
-- <<docs/haddock/Geometry/Algorithms/Clipping.hs/hatched_polygon.svg>>
hatch
    :: Polygon
    -> Angle -- ^ Direction in which the lines will point. @'deg' 0@ is parallel to the x axis.
    -> Double -- ^ Distance between shading lines
    -> [Line]
hatch polygon angle hatchInterval = do
    let polygonAligned = transform (rotate (negateV angle)) polygon
    horizontalScissors <- do
        let BoundingBox (Vec2 xLo yLo) (Vec2 xHi yHi) = boundingBox polygonAligned
        y <- takeWhile (< yHi) (tail (iterate (+ hatchInterval) yLo))
        pure (Line (Vec2 xLo y) (Vec2 xHi y))
    horizontalHatches <- [line | (line, LineInsidePolygon) <- clipPolygonWithLine polygonAligned horizontalScissors]
    pure (transform (rotate angle) horizontalHatches)
