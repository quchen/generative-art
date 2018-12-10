module Geometry.Processes.RandomCut (
      randomCutS
    , randomCutProcess
) where



import Control.Monad.Trans.State
import Data.List
import System.Random

import Geometry



newtype CutSeed = CutSeed Int
    deriving (Eq, Ord, Show)

boundingBoxPoly :: Polygon -> (Vec2, Vec2)
boundingBoxPoly (Polygon []) = error "Empty polygon"
boundingBoxPoly (Polygon (c0 : corners)) = foldl' minMax (c0, c0) corners
  where
    minMax (vMin, vMax) v = (min vMin v, max vMax v)

randomR' :: Random r => (r, r) -> State StdGen r
randomR' range = do
    gen <- get
    let (x, gen') = randomR range gen
    put gen'
    pure x

randomCutS
    :: ([Polygon] -> Bool)
    -> Polygon
    -> State StdGen [Polygon]
randomCutS acceptCut polygon = findGoodCut
  where
    (Vec2 minX minY, Vec2 maxX maxY) = boundingBoxPoly polygon

    findGoodCut = do
        p <- Vec2 <$> randomR' (minX, maxX) <*> randomR' (minY, maxY)
        angle <- fmap deg (randomR' (0, 360))
        let cutResult = cutPolygon (angledLine p angle (Distance 1)) polygon
        if acceptCut cutResult
            then pure cutResult
            else findGoodCut

randomCutProcess
    :: (Polygon -> Bool)   -- ^ Recursively subdivide the current polygon?
    -> ([Polygon] -> Bool) -- ^ Accept the cut result, or retry with a different random cut line?
    -> Polygon             -- ^ Initial polygon, cut only if the recursion predicate applies
    -> StdGen
    -> ([Polygon], StdGen)
randomCutProcess recurse acceptCut polygon
  = runState (randomCutProcessS recurse acceptCut polygon)

randomCutProcessS
    :: (Polygon -> Bool)
    -> ([Polygon] -> Bool)
    -> Polygon
    -> State StdGen [Polygon]
randomCutProcessS recurse acceptCut polygon
    | recurse polygon = do
        cutResult <- randomCutS acceptCut polygon
        recurses <- traverse (randomCutProcessS recurse acceptCut) cutResult
        pure (concat recurses)
    | otherwise = pure [polygon]
