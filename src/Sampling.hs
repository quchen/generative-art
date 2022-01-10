{-# LANGUAGE RecordWildCards #-}
module Sampling (
      PoissonDiscProperties(..)
    , poissonDisc
) where

import Data.Map.Strict as M
import Data.Set as S
import System.Random.MWC (GenIO, UniformRange (uniformRM))

import Geometry
import Control.Monad.Trans.State (StateT, gets, modify, execStateT)
import Data.Traversable (for)
import Data.Maybe (maybeToList)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)


data PoissonDiscProperties = PoissonDisc
    { width :: Int
    , height :: Int
    , radius :: Double
    , gen :: GenIO
    , k :: Int
    }

poissonDisc :: PoissonDiscProperties -> IO [Vec2]
poissonDisc properties = result <$> execStateT poissonDiscInternal initialState
  where
    initialState = State { grid = mempty, activeSamples = mempty, result = mempty, ..}

data PoissonDiscState = State
    { properties :: PoissonDiscProperties
    , grid :: Map (Int, Int) Vec2
    , activeSamples :: Set Vec2
    , result :: [Vec2]
    }

poissonDiscInternal :: StateT PoissonDiscState IO ()
poissonDiscInternal = do
    PoissonDisc{..} <- gets properties
    initialSample <- liftIO $ do
        x <- uniformRM (0, fromIntegral width) gen
        y <- uniformRM (0, fromIntegral height) gen
        pure (Vec2 x y)
    addSample initialSample
    nextSample

nextSample :: StateT PoissonDiscState IO ()
nextSample = do
    numActiveSamples <- gets (S.size . activeSamples)
    when (numActiveSamples > 0) $ do
        randomSampleIndex <- gets (gen . properties) >>= uniformRM (0, numActiveSamples - 1)
        randomSample <- gets (S.elemAt randomSampleIndex . activeSamples)
        r <- gets (radius . properties)
        attempts <- gets (k . properties)

        let loop 0 = pure Nothing
            loop n = do
                candidate <- nextCandidate randomSample
                neighbours <- neighbouringSamples candidate
                let distance p q = norm (q -. p)
                if any (\p -> distance candidate p <= Distance r) neighbours
                    then loop (n-1)
                    else pure (Just candidate)

        newSample <- loop attempts

        case newSample of
            Nothing -> retireSample randomSample
            Just sample -> addSample sample

        nextSample

nextCandidate :: Vec2 -> StateT PoissonDiscState IO Vec2
nextCandidate v = do
    PoissonDisc{..} <- gets properties
    rCoord <- liftIO $ uniformRM (radius, 2 * radius) gen
    phiCoord <- liftIO $ uniformRM (0, 2 * pi) gen
    let candidate@(Vec2 x y) = v +. polar (rad phiCoord) (Distance rCoord)
    if x >= 0 && x <= fromIntegral width && y >= 0 && y <= fromIntegral height
        then pure candidate
        else nextCandidate v


addSample :: Vec2 -> StateT PoissonDiscState IO ()
addSample sample = do
    cell <- gridCell sample
    modify (\s -> s { grid = M.insert cell sample (grid s)})
    modify (\s -> s { activeSamples = S.insert sample (activeSamples s) })

retireSample :: Vec2 -> StateT PoissonDiscState IO ()
retireSample sample = do
    modify (\s -> s { activeSamples = S.delete sample (activeSamples s) })
    modify (\s -> s { result = sample : result s })

-- A cell in the grid has a side length of r/sqrt(2). Therefore, to detect
-- collisions, we need to search a space of 5x5 cells at max.
neighbouringSamples :: Vec2 -> StateT PoissonDiscState IO [Vec2]
neighbouringSamples v = do
    (x, y) <- gridCell v
    let minX = x - 2
        maxX = x + 2
        minY = y - 2
        maxY = y + 2
    neighbours <- for [(cellX, cellY) | cellX <- [minX..maxX], cellY <- [minY..maxY]] $ \cell ->
        maybeToList . M.lookup cell <$> gets grid
    pure (concat neighbours)

gridCell :: Vec2 -> StateT PoissonDiscState IO (Int, Int)
gridCell (Vec2 x y) = do
    r <- gets (radius . properties)
    pure (floor (x/r), floor (y/r))
