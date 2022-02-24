module Geometry.Algorithms.Sampling (
    -- * Poisson-Disc sampling
    PoissonDiscParams(..)
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
import Data.Default.Class

import Geometry



-- | Configuration for 'poissonDisc' sampling.
data PoissonDiscParams = PoissonDiscParams
    { _poissonWidth  :: Int    -- ^ 'def'ault 256.
    , _poissonHeight :: Int    -- ^ 'def'ault 256.
    , _poissonRadius :: Double -- ^ Minimum distance between points. 'def'ault 100.
    , _poissonK      :: Int    -- ^ Point density. Per known point, how many random points in the vicinity should be tried? 'def'ault 32.
    }

instance Default PoissonDiscParams where
    def = PoissonDiscParams
        { _poissonWidth  = 256
        , _poissonHeight = 256
        , _poissonRadius = 100
        , _poissonK      = 32
        }

-- | Sample points using the Poisson Disc algorithm, which yields a visually
-- uniform distribution. This is opposed to uniformly distributed points yield
-- clumps and empty areas, which is often undesirable for generative art.
--
-- <<docs/sampling/poisson-disc.svg>>
--
-- === Example code
--
-- The \(r=8\) picture is based on the following code:
--
-- @
-- points :: ['Vec2']
-- points = 'Control.Monad.ST.runST' $ do
--     gen <- 'create'
--     'poissonDisc' gen 'PoissonDiscParams'
--         { '_poissonWidth'  = 80
--         , '_poissonHeight' = 80
--         , '_poissonRadius' = 8
--         , '_poissonK'      = 4
--         }
-- @
poissonDisc
    :: PrimMonad m
    => Gen (PrimState m) -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> PoissonDiscParams
    -> m [Vec2]
poissonDisc gen properties = _result <$> execStateT poissonDiscInternal initialState
  where
    initialState = PoissonDiscState
        { _properties    = properties
        , _gen           = gen
        , _grid          = mempty
        , _activeSamples = mempty
        , _result        = mempty
        }

data PoissonDiscState s = PoissonDiscState
    { _properties    :: PoissonDiscParams
    , _gen           :: Gen s
    , _grid          :: Map (Int, Int) Vec2
    , _activeSamples :: Set Vec2
    , _result        :: [Vec2]
    }

poissonDiscInternal :: PrimMonad m => StateT (PoissonDiscState (PrimState m)) m ()
poissonDiscInternal = do
    PoissonDiscState{..} <- get
    let PoissonDiscParams{..} = _properties
    initialSample <- lift $ do
        x <- uniformRM (0, fromIntegral _poissonWidth) _gen
        y <- uniformRM (0, fromIntegral _poissonWidth) _gen
        pure (Vec2 x y)
    addSample initialSample
    nextSample

nextSample :: PrimMonad m => StateT (PoissonDiscState (PrimState m)) m ()
nextSample = do
    numActiveSamples <- gets (S.size . _activeSamples)
    when (numActiveSamples > 0) $ do
        randomSampleIndex <- gets _gen >>= uniformRM (0, numActiveSamples - 1)
        randomSample <- gets (S.elemAt randomSampleIndex . _activeSamples)
        r <- gets (_poissonRadius . _properties)

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
    PoissonDiscState{..} <- get
    let PoissonDiscParams{..} = _properties
    phi0 <- lift (rad <$> uniformRM (0, 2*pi) _gen)
    let deltaPhi = rad (2*pi / fromIntegral _poissonK)
        candidates = filter (isWithinBounds _poissonWidth _poissonHeight)
            [ v +. polar (phi0 +. i *. deltaPhi) r
            | let r = _poissonRadius + 0.000001
            , i <- [1..fromIntegral _poissonK] ]
    pure candidates

  where
    isWithinBounds w h (Vec2 x y) = x >= 0 && x <= fromIntegral w && y >= 0 && y <= fromIntegral h

addSample :: PrimMonad m => Vec2 -> StateT (PoissonDiscState (PrimState m)) m ()
addSample sample = do
    cell <- gridCell sample
    modify (\s -> s { _grid = M.insert cell sample (_grid s)})
    modify (\s -> s { _activeSamples = S.insert sample (_activeSamples s) })

retireSample :: PrimMonad m => Vec2 -> StateT (PoissonDiscState (PrimState m)) m ()
retireSample sample = do
    modify (\s -> s { _activeSamples = S.delete sample (_activeSamples s) })
    modify (\s -> s { _result = sample : _result s })

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
        pure (maybeToList . M.lookup (cellX, cellY) <$> gets _grid)
    pure (concat neighbours)

gridCell :: PrimMonad m => Vec2 -> StateT (PoissonDiscState (PrimState m)) m (Int, Int)
gridCell (Vec2 x y) = do
    r <- gets (_poissonRadius . _properties)
    let gridSize = r / sqrt 2
    pure (floor (x/gridSize), floor (y/gridSize))

-- | @'uniformlyDistributedPoints' gen width height count@ generates @count@
-- random points within a rectangle of @width@ x @height@.
--
-- Create 100 points in a 64*64 square:
--
-- @
-- points :: 'Vector' 'Vec2'
-- points = 'Control.Monad.ST.runST' $ do
--     gen <- 'create'
--     'uniformlyDistributedPoints' gen 64 64 100
-- @
uniformlyDistributedPoints
    :: PrimMonad m
    => Gen (PrimState m) -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> Int               -- ^ Width
    -> Int               -- ^ Height
    -> Int               -- ^ Number of points
    -> m (Vector Vec2)
uniformlyDistributedPoints gen width height count = V.replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width) (randomCoordinate height)
    randomCoordinate mx = uniformR (0, fromIntegral mx) gen

-- | @'uniformlyDistributedPoints' gen (width, sigmaX) (height, sigmaY) count@
-- generates @count@ normal distributed random points within a rectangle of
-- @width@ x @height@, with the given standard deviations.
--
-- Note: This is a rejection algorithm. If you choose the standard deviation much
-- higher than the height or width, performance will deteriorate as more and more
-- points are rejected.
--
-- Create 100 Gaussian points in a 64*64 square with standard deviation \(\sigma=10\):
--
-- @
-- points :: 'Vector' 'Vec2'
-- points = 'Control.Monad.ST.runST' $ do
--     gen <- 'create'
--     'uniformlyDistributedPoints' gen (64, 10) (64, 10) 100
-- @
gaussianDistributedPoints
    :: PrimMonad m
    => Gen (PrimState m) -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> (Int, Double)     -- ^ Width, \(\sigma_x\)
    -> (Int, Double)     -- ^ Height, \(\sigma_y\)
    -> Int               -- ^ Number of points
    -> m (Vector Vec2)
gaussianDistributedPoints gen (width, sigmaX) (height, sigmaY) count = V.replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width sigmaX) (randomCoordinate height sigmaY)
    randomCoordinate mx sigma = do
        coord <- normal (fromIntegral mx/2) sigma gen
        if coord < 0 || coord > fromIntegral mx
            then randomCoordinate mx sigma
            else pure coord
