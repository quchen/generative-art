module Geometry.Algorithms.Path.Dijkstra (DijkstraParams(..), dijkstra) where

import Control.Monad (when)
import Control.Monad.Trans.State
import Data.Foldable (for_)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Geometry hiding (Grid)



data DijkstraParams = Dijkstra
    { costFunction :: Vec2 -> Double
    -- ^ Cost function for a step at this point. Should be strictly positive:
    -- A cost of one is equivalent to the cartesian distance.

    , width :: Double
    -- ^ Width of the area to be sampled

    , height :: Double
    -- ^ Height of the area to be sampled

    , step :: Double
    -- ^ Step size of the sampling grid. With smaller step size, the more,
    -- solutions will be more accurate, but will take longer to compute.
    }

-- The Dijkstra algorithm for finding a path through a field.
--
-- It samples the given cost function on a grid, and tries to find the path
-- with the lowest cost from a starting point to an end point.
--
-- Due to the grid structure, the resulting paths will not be straight, but
-- can contain unnecessary bends, especially through even terrain. You can use
-- 'Geometry.Algorithms.Path.Optimize.optimizePath' with the same cost function
-- to smoothen and optimize the path.
dijkstra
    :: DijkstraParams
    -> Vec2 -- ^ Start point
    -> Vec2 -- ^ End point
    -> [Vec2]
dijkstra params a b = flip evalState initialState $ go start
  where
    go p
        | p == end = fmap (toVec2 params) . snd <$> gets ((M.! p) . distanceMap)
        | otherwise = do
            visit params p
            next <- gets (minimumBy (comparing (fst . snd)) . M.toList . distanceMap)
            go (fst next)

    start = fromVec2 params a
    end   = fromVec2 params b
    initialState = State { distanceMap = M.singleton start (0, [start]), visited = S.empty }

fromVec2 :: DijkstraParams -> Vec2 -> (Int, Int)
fromVec2 Dijkstra{..} (Vec2 x y) = (round (x/step), round (y/step))

toVec2 :: DijkstraParams -> (Int, Int) -> Vec2
toVec2 Dijkstra{..} (x, y) = Vec2 (fromIntegral x * step) (fromIntegral y * step)

data DijkstraState = State
    { distanceMap :: M.Map (Int, Int) (Double, [(Int, Int)])
    , visited :: S.Set (Int, Int)
    }

neighbours :: DijkstraParams -> (Int, Int) -> [(Int, Int)]
neighbours Dijkstra{..} (x, y) =
    [ (x', y')
    | x' <- [x-1 .. x+1]
    , y' <- [y-1 .. y+1]
    , x' >= 0 && fromIntegral x' < width/step && y' >= 0 && fromIntegral y' < height/step
    ]

visit :: DijkstraParams -> (Int, Int) -> State DijkstraState ()
visit params@Dijkstra{..} p = do
    let w = costFunction (toVec2 params p)
    (currentDistance, currentTrajectory) <- gets ((M.! p) . distanceMap)
    alreadyVisited <- gets visited
    for_ (neighbours params p) $ \p' -> when (p' `S.notMember` alreadyVisited) $ do
        let d = distance p p' * w
        gets (M.lookup p' . distanceMap) >>= \case
            Just (neighbourDistance, _) | currentDistance + d >= neighbourDistance -> pure ()
            _otherwise -> modify $ \s -> s
                { distanceMap = M.insert p' (currentDistance + d, currentTrajectory ++ [p']) (distanceMap s)}
    modify $ \s -> s
       { visited = S.insert p alreadyVisited
       , distanceMap = M.delete p (distanceMap s)
       }

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x, y) (x', y') = sqrt (fromIntegral ((x-x')^2 + (y-y')^2))
