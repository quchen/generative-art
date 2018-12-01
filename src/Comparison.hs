-- | Intention: compare generated image with a predefined one to approximate
-- pictures using genetic algorithms
module Comparison (
      bhattacharyyaDistance
    , hellingerDistance
) where



import           Data.Vector (Vector)
import qualified Data.Vector as V



bhattacharyyaDistance :: Vector Double -> Vector Double -> Double
bhattacharyyaDistance xs ys = - log (bhattacharyyaCoefficient xs ys)

bhattacharyyaCoefficient :: Vector Double -> Vector Double -> Double
bhattacharyyaCoefficient xs ys = V.foldl' (+) 0 (V.zipWith (\x y -> sqrt (x*y)) xs ys)

hellingerDistance :: Vector Double -> Vector Double -> Double
hellingerDistance xs ys = sqrt (1 - bhattacharyyaCoefficient xs ys)
