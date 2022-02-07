{-# LANGUAGE RecordWildCards #-}
module Geometry.Algorithms.Sampling (
    -- * Poisson-Disc sampling
    PoissonDiscProperties(..)
    , poissonDisc

    -- * Other distributions
    , uniformlyDistributedPoints
    , gaussianDistributedPoints
) where



import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Set                        (Set)
import qualified Data.Set                        as S
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           System.Random.MWC
import           System.Random.MWC.Distributions

import Geometry



data PoissonDiscProperties s = PoissonDisc
    { width :: Int
    , height :: Int
    , radius :: Double
    , gen :: GenST s
    , k :: Int
    }

-- | <<docs/sampling/poisson-disc.svg>>
poissonDisc :: PrimMonad m => PoissonDiscProperties (PrimState m) -> m [Vec2]
poissonDisc properties = result <$> execStateT poissonDiscInternal initialState
  where
    initialState = State { grid = mempty, activeSamples = mempty, result = mempty, ..}

data PoissonDiscState s = State
    { properties    :: PoissonDiscProperties s
    , grid          :: Map (Int, Int) Vec2
    , activeSamples :: Set Vec2
    , result        :: [Vec2]
    }

poissonDiscInternal :: PrimMonad m => StateT (PoissonDiscState (PrimState m)) m ()
poissonDiscInternal = do
    PoissonDisc{..} <- gets properties
    initialSample <- lift $ do
        x <- uniformRM (0, fromIntegral width) gen
        y <- uniformRM (0, fromIntegral height) gen
        pure (Vec2 x y)
    addSample initialSample
    nextSample

nextSample :: PrimMonad m => StateT (PoissonDiscState (PrimState m)) m ()
nextSample = do
    numActiveSamples <- gets (S.size . activeSamples)
    when (numActiveSamples > 0) $ do
        randomSampleIndex <- gets (gen . properties) >>= uniformRM (0, numActiveSamples - 1)
        randomSample <- gets (S.elemAt randomSampleIndex . activeSamples)
        r <- gets (radius . properties)

        candidates <- nextCandidates randomSample

        let loop [] = pure Nothing
            loop (candidate:cs) = do
                neighbours <- neighbouringSamples candidate
                let distance p q = norm (q -. p)
                if any (\p -> distance candidate p <= r) neighbours
                    then loop cs
                    else pure (Just candidate)

        newSample <- loop candidates

        case newSample of
            Nothing -> retireSample randomSample
            Just sample -> addSample sample

        nextSample

-- | http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
nextCandidates :: PrimMonad m => Vec2 -> StateT (PoissonDiscState (PrimState m)) m [Vec2]
nextCandidates v = do
    PoissonDisc{..} <- gets properties
    phi0 <- lift (rad <$> uniformRM (0, 2*pi) gen)
    let deltaPhi = rad (2*pi / fromIntegral k)
        candidates = filter (isWithinBounds width height)
            [ v +. polar (phi0 +. i *. deltaPhi) r
            | let r = radius + 0.000001
            , i <- [1..fromIntegral k] ]
    pure candidates

  where
    isWithinBounds w h (Vec2 x y) = x >= 0 && x <= fromIntegral w && y >= 0 && y <= fromIntegral h

addSample :: PrimMonad m => Vec2 -> StateT (PoissonDiscState (PrimState m)) m ()
addSample sample = do
    cell <- gridCell sample
    modify (\s -> s { grid = M.insert cell sample (grid s)})
    modify (\s -> s { activeSamples = S.insert sample (activeSamples s) })

retireSample :: PrimMonad m => Vec2 -> StateT (PoissonDiscState (PrimState m)) m ()
retireSample sample = do
    modify (\s -> s { activeSamples = S.delete sample (activeSamples s) })
    modify (\s -> s { result = sample : result s })

-- A cell in the grid has a side length of r/sqrt(2). Therefore, to detect
-- collisions, we need to search a space of 5x5 cells at max.
neighbouringSamples :: PrimMonad m => Vec2 -> StateT (PoissonDiscState (PrimState m)) m [Vec2]
neighbouringSamples v = do
    (x, y) <- gridCell v
    let minX = x - 2
        maxX = x + 2
        minY = y - 2
        maxY = y + 2
    neighbours <- sequence $ do
        cellX <- [minX..maxX]
        cellY <- [minY..maxY]
        pure (maybeToList . M.lookup (cellX, cellY) <$> gets grid)
    pure (concat neighbours)

gridCell :: PrimMonad m => Vec2 -> StateT (PoissonDiscState (PrimState m)) m (Int, Int)
gridCell (Vec2 x y) = do
    r <- gets (radius . properties)
    let gridSize = r / sqrt 2
    pure (floor (x/gridSize), floor (y/gridSize))

-- | @'uniformlyDistributedPoints' gen width height count@ generates @count@
-- random points within a rectangle of @width@ x @height@.
uniformlyDistributedPoints :: PrimMonad m => Gen (PrimState m) -> Int -> Int -> Int -> m (Vector Vec2)
uniformlyDistributedPoints gen width height count = V.replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width) (randomCoordinate height)
    randomCoordinate mx = uniformR (0, fromIntegral mx) gen

-- | @'uniformlyDistributedPoints' gen (width, sigmaX) (height, sigmaY) count@
-- generates @count@ normal distributed random points within a rectangle of
-- @width@ x @height@, with a the given standard deviations.
--
-- Note: This is a rejection algorithm. If you choose the standard deviation
-- much higher than the height or width, performance will deteriorate as more
-- and more points are rejected.
gaussianDistributedPoints :: PrimMonad m => Gen (PrimState m) -> (Int, Double) -> (Int, Double) -> Int -> m (Vector Vec2)
gaussianDistributedPoints gen (width, sigmaX) (height, sigmaY) count = V.replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width sigmaX) (randomCoordinate height sigmaY)
    randomCoordinate mx sigma = do
        coord <- normal (fromIntegral mx/2) sigma gen
        if coord < 0 || coord > fromIntegral mx
            then randomCoordinate mx sigma
            else pure coord
