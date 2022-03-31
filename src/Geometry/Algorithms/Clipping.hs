module Geometry.Algorithms.Clipping (
      cutLineWithLine
    , CutLine(..)
    , cutPolygon

    , LineType(..)
    , clipPolygonWithLine

    , intersectionPP
    , unionPP
    , differencePP

    , hatch
) where



import Geometry.Algorithms.Clipping.Internal
import Geometry.Algorithms.Clipping.PolygonPolygon
import Geometry.Core

-- $setup
-- >>> import Draw
-- >>> import Graphics.Rendering.Cairo

-- | Add shading lines to a polygon. This is especially useful for a pen plotter to
-- do shading, hence the name.
--
-- >>> :{
-- haddockRender "Geometry/Algorithms/Clipping.hs/hatchd_polygon.svg" 100 100 $ do
--     let polygon = Polygon [Vec2 10 10, Vec2 90 10, Vec2 90 90, Vec2 10 90]
--     let shading = hatch polygon (deg 30) 10
--     sketch polygon
--     for_ shading sketch
--     stroke
-- :}
-- docs/haddock/Geometry/Algorithms/Clipping.hs/hatchd_polygon.svg
--
-- <<docs/haddock/Geometry/Algorithms/Clipping.hs/hatchd_polygon.svg>>
hatch
    :: Polygon
    -> Angle -- ^ Direction in which the lines will point. @'deg' 0@ is parallel to the x axis.
    -> Double -- ^ Distance between shading lines
    -> [Line]
hatch polygon angle hatchInterval = do

    -- We first fill the whole bounding box perimeter with lines evenly spaced
    -- horizontal lines. Afterwards we rotate them around the bounding box center
    -- by the right angle, and use those for clipping.
    let bb = boundingBox polygon
        bbCenter = boundingBoxCenter bb
        bbCircleR = let (bbW, bbH) = boundingBoxSize bb in norm (Vec2 bbW bbH) / 2
    scissorsHorizontal <- do
        let graceInterval = 1
        shiftAmount <- takeWhile
            (<= bbCircleR+graceInterval)
            (iterate (+hatchInterval) (-bbCircleR-graceInterval))
        pure (transform (translate (Vec2 0 shiftAmount)) (angledLine bbCenter (deg 0) 100))
    let scissors = transform (rotateAround bbCenter angle) scissorsHorizontal

    [line | (line, LineInsidePolygon) <- clipPolygonWithLine polygon scissors]
