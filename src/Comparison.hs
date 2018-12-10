-- | Intention: compare generated image with a predefined one to approximate
-- pictures using genetic algorithms
module Comparison (
      bhattacharyyaDistance
    , hellingerDistance
    , minMaxAreaRatio
) where



import           Data.Vector (Vector)
import qualified Data.Vector as V

import Geometry.Core



bhattacharyyaDistance :: Vector Double -> Vector Double -> Double
bhattacharyyaDistance xs ys = - log (bhattacharyyaCoefficient xs ys)

bhattacharyyaCoefficient :: Vector Double -> Vector Double -> Double
bhattacharyyaCoefficient xs ys = V.foldl' (+) 0 (V.zipWith (\x y -> sqrt (x*y)) xs ys)

hellingerDistance :: Vector Double -> Vector Double -> Double
hellingerDistance xs ys = sqrt (1 - bhattacharyyaCoefficient xs ys)

-- | Calculate the min/max ratio of the areas of a list of polygons. Useful to
-- build cutoff predicates with, e.g.
--
-- @
-- \polys -> 'minMaxAreaRatio' polys >= 1/3
-- @
minMaxAreaRatio :: [Polygon] -> Double
minMaxAreaRatio cutResult
  = let cutResultAreas = map polygonArea cutResult
        Area minA = minimum cutResultAreas
        Area maxA = maximum cutResultAreas
    in minA / maxA

-- | Measure of how big the height of the minimal bounding box of a polygon is
-- compared to its width.
excentricity :: Polygon -> Double
excentricity = undefined
