module Geometry.Algorithms.Cut (
      cutLine
    , CutLine(..)
    , cutPolygon

    , LineType(..)
    , clipPolygonWithLine

    , shade
) where



import Geometry.Core
import Geometry.Algorithms.Cut.Internal



-- | Add shading lines to a polygon. This is especially useful for a pen plotter to
-- do shading, hence the name.
shade
    :: Polygon
    -> Angle -- ^ Direction in which the lines will point. @'deg' 0@ is parallel to the x axis.
    -> Double -- ^ Distance between shading line
    -> [Line]
shade polygon angle shadeInterval = do

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
            (iterate (+shadeInterval) (-bbCircleR-graceInterval))
        pure (transform (translate (Vec2 0 shiftAmount)) (angledLine bbCenter (deg 0) 100))
    let scissors = transform (rotateAround bbCenter angle) scissorsHorizontal

    [line | (line, LineInsidePolygon) <- clipPolygonWithLine polygon scissors]
