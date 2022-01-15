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



-- | The Bhattacharyya distance measures the similarity of two probability distributions.
--
-- I’ve added it here because I’m hoping to use it to implement a distance metric
-- between two pictures at some point.
--
-- \[
--   D_{B}(p,q)=-\ln \left(BC(p,q)\right) \\
--   BC(p,q)=\sum _{{x\in X}}{\sqrt {p(x)q(x)}}
-- \]
--
-- See https://en.wikipedia.org/wiki/Bhattacharyya_distance
bhattacharyyaDistance :: Vector Double -> Vector Double -> Double
bhattacharyyaDistance xs ys = - log (bhattacharyyaCoefficient xs ys)

-- | The Bhattacharyya coefficient is an approximate measurement of the amount of overlap between two statistical samples.
--
-- \[
--   BC(p,q)=\sum _{{x\in X}}{\sqrt {p(x)q(x)}}
-- \]
--
-- See https://en.wikipedia.org/wiki/Bhattacharyya_distance#Bhattacharyya_coefficient
bhattacharyyaCoefficient :: Vector Double -> Vector Double -> Double
bhattacharyyaCoefficient xs ys = V.foldl' (+) 0 (V.zipWith (\x y -> sqrt (x*y)) xs ys)

-- | The Hellinger distance measures the similarity of two probability distributions.
--
-- I’ve added it here because I’m hoping to use it to implement a distance metric
-- between two pictures at some point.
--
-- \[
--   H(p,q)={\sqrt {1-BC(p,q)}} \\
--   BC(p,q)=\sum _{{x\in X}}{\sqrt {p(x)q(x)}}
-- \]
--
-- See https://en.wikipedia.org/wiki/Hellinger_distance
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
