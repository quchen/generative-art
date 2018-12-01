module Geometry.Processes.RandomCut (cutProcess) where




import System.Random
import Data.List


import Geometry



newtype CutSeed = CutSeed Int
    deriving (Eq, Ord, Show)

boundingBoxPoly :: Polygon -> (Vec2, Vec2)
boundingBoxPoly (Polygon []) = error "Empty polygon"
boundingBoxPoly (Polygon (c0 : corners)) = foldl' minMax (c0, c0) corners
  where
    minMax (vMin, vMax) v = (min vMin v, max vMax v)

cutProcess :: StdGen -> Polygon -> ([Polygon], StdGen)
cutProcess initialGen polygon = findGoodCut initialGen
  where
    (Vec2 minX minY, Vec2 maxX maxY) = boundingBoxPoly polygon

    findGoodCut g
      = let (x, g1) = randomR (minX, maxX) g
            (y, g2) = randomR (minY, maxY) g1
            (angle, g3) = randomR (0, 360) g2

            cutResult = cutPolygon (angledLine (Vec2 x y) (deg angle) (Distance 1)) polygon
            accept = let cutResultAreas = map polygonArea cutResult
                         Area minA = minimum cutResultAreas
                         Area maxA = maximum cutResultAreas
                     in maxA / minA <= 3
        in if accept
            then (cutResult, g3)
            else findGoodCut g3

cutProcess'
    :: StdGen
    -> Polygon
    -> ([Polygon] -> Bool)
    -> ([Polygon] -> Bool)
    -> ([Polygon], StdGen)
cutProcess' initialGen polygon acceptCut recurse = undefined
