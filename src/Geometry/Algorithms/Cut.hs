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



shade :: Polygon -> Angle -> Double -> [Line]
shade polygon angle shadeDistance = do
    let BoundingBox bbMin _ = boundingBox polygon
        baseScissors = angledLine bbMin angle 1
    scissors <- do
        shift <- map (shadeDistance *) [0..100]
        pure (shiftLinePerpendicular baseScissors shift)

    [line | (line, _) <- clipPolygonWithLine polygon scissors]

shiftLinePerpendicular :: Line -> Double -> Line
shiftLinePerpendicular line@(Line start end) amount =
    let lineAngle = angleOfLine line
        lineNormalized = Line start (transform (rotateAround start (negateV lineAngle)) end)
        Line shiftedStart shiftedEnd = transform (translate (Vec2 0 amount)) lineNormalized
        lineRerotated = Line shiftedStart (transform (rotateAround shiftedStart lineAngle) shiftedEnd)
    in lineRerotated
