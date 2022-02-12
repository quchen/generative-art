module Geometry.Algorithms.Path.Optimize (optimizePath) where

import Control.Monad.ST
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.Distributions

import Geometry



optimizePath
    :: Int -- ^ Number of iterations, e.g. 20000 (the more points in the trajectory, the more steps are required).
    -> Double -- ^ Step size relative to the distance between two points. My experience is that 0.01 is a good value.
    -> (Vec2 -> Double) -- ^ Cost function for a step at this point.
    -> V.Vector Vec2 -- ^ Input path
    -> V.Vector Vec2
optimizePath iterations stepSize costFunction path = runST $ do
    gen <- create
    iterateM iterations (optimizationStep stepSize costFunction gen) path

optimizationStep :: Double -> (Vec2 -> Double) -> GenST s -> V.Vector Vec2 -> ST s (V.Vector Vec2)
optimizationStep d f gen ps = do
    pos <- uniformRM (1, V.length ps - 2) gen
    nudge <- normal 0 d gen
    let item = ps V.! pos
        [a, b, c] = V.toList (V.slice (pos-1) 3 ps)
        nudgeDirection = let Vec2 x y = (c -. a) /. 2 in Vec2 y (-x)
        b' = (a+.c) /. 2 +. nudge *. nudgeDirection
    pure $ if segmentLength a b' c < segmentLength a b c
        then ps V.// [(pos, b')]
        else ps
  where
    segmentLength a b c = (f a + f b) * norm (b -. a) + (f b + f c) * norm (c -. b)

gaussianVec2 :: Vec2 -> Double -> GenST s -> ST s Vec2
gaussianVec2 (Vec2 muX muY) sigma gen = Vec2 <$> normal muX sigma gen <*> normal muY sigma gen

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 _ a = pure a
iterateM n f a = f a >>= iterateM (n-1) f
