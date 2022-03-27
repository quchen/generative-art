module Geometry.Algorithms.Cut (
      cutLine
    , CutLine(..)
    , cutPolygon

    , LineType(..)
    , clipPolygonWithLine

    , shade
) where



import Data.Maybe

import Geometry.Core
import Geometry.Algorithms.Cut.Internal



shade :: Polygon -> Angle -> Double -> [Line]
shade polygon angle shadeInterval = do
    let bb = boundingBox polygon
        baseScissors = angledLine (boundingBoxCenter bb) angle 1
        offset = polar (angle +. deg 90) shadeInterval
    scissors <- do
        let direction1 = map (\n -> transform (translate (n *. offset)) baseScissors) [0..]
            direction2 = map (\n -> transform (translate (n *. offset)) baseScissors) [-1,-2..]
            stillRelevant line = isJust (boundingBoxIntersection line bb)
        takeWhile stillRelevant direction1 <> takeWhile stillRelevant direction2

    [line | (line, _) <- clipPolygonWithLine polygon scissors]
