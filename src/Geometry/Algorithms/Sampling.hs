module Geometry.Algorithms.Sampling (
    -- * Poisson-Disc sampling
    PoissonDiscParams(..)
    , poissonDisc

    -- * Other distributions
    , uniformlyDistributedPoints
    , gaussianDistributedPoints
) where



import           Control.Applicative
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader
import           Data.Default.Class
import           Data.Heap                       (Entry, Heap)
import qualified Data.Heap                       as H
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           System.Random.MWC
import           System.Random.MWC.Distributions

import Geometry



-- | Configuration for 'poissonDisc' sampling.
data PoissonDiscParams = PoissonDiscParams
    { _poissonWidth  :: Int    -- ^ 'def'ault 256.
    , _poissonHeight :: Int    -- ^ 'def'ault 256.
    , _poissonRadius :: Double -- ^ Minimum distance between points. 'def'ault 100.
    , _poissonK      :: Int    -- ^ How many attempts to find a neighbouring point should be made?
                               --   The higher this is, the denser the resulting point set will be.
    }

instance Default PoissonDiscParams where
    def = PoissonDiscParams
        { _poissonWidth  = 256
        , _poissonHeight = 256
        , _poissonRadius = 100
        , _poissonK      = 32
        }

newtype PoissonT m a = PoissonT {runPoissonT :: ReaderT PoissonDiscParams (StateT (PoissonDiscState (PrimState m)) m) a}

instance Monad m => Functor (PoissonT m) where
    fmap f (PoissonT a) = PoissonT (fmap f a)

instance Monad m => Applicative (PoissonT m) where
    pure x = PoissonT (pure x)
    PoissonT mf <*> PoissonT mx = PoissonT (mf <*> mx)

instance Monad m => Monad (PoissonT m) where
    PoissonT a >>= g = PoissonT (a >>= runPoissonT . g)

instance MonadTrans PoissonT where
    lift = PoissonT . lift . lift

execPoissonT
    :: Monad m
    => PoissonT m a                   -- ^ Action
    -> PoissonDiscParams              -- ^ Config
    -> PoissonDiscState (PrimState m) -- ^ Initial state
    -> m (PoissonDiscState (PrimState m))
execPoissonT (PoissonT action) params initialState = execStateT (runReaderT action params) initialState

asksParams :: Monad m => (PoissonDiscParams -> a) -> PoissonT m a
asksParams f = PoissonT (asks f)

getsState :: Monad m => (PoissonDiscState (PrimState m) -> a) -> PoissonT m a
getsState proj = PoissonT (lift (gets proj))

modifyState
    :: Monad m
    => (PoissonDiscState (PrimState m) -> PoissonDiscState (PrimState m))
    -> PoissonT m ()
modifyState f = PoissonT (lift (modify f))

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
poissonDisc gen params = do
    let PoissonDiscParams{..} = params
        minV = zero
        maxV = Vec2 (fromIntegral _poissonWidth) (fromIntegral _poissonHeight)
    initialSample <- uniformRM (minV, maxV) gen
    let initialState = PoissonDiscState
            { _gen           = gen
            , _grid          = mempty
            , _activeSamples = mempty
            , _result        = mempty
            , _initialPoint  = initialSample
            }

    _result <$> execPoissonT (addSample initialSample >> sampleLoop) params initialState

data PoissonDiscState s = PoissonDiscState
    { _gen           :: Gen s
    , _grid          :: Map (Int, Int) Vec2
    , _activeSamples :: Heap (Entry Double Vec2)
    , _result        :: [Vec2]
    , _initialPoint  :: Vec2
    }

sampleLoop :: PrimMonad m => PoissonT m ()
sampleLoop = do
    heap <- getsState _activeSamples
    case H.uncons heap of
        Nothing -> pure ()
        Just (H.Entry _ closestActiveSample, heap') -> do
            r <- asksParams _poissonRadius

            candidates <- nextCandidates closestActiveSample

            let validPoint candidate = do
                    neighbours <- neighbouringSamples candidate
                    pure (not (any (\p -> norm (candidate -. p) <= r) neighbours))

            newSample <- findM validPoint candidates

            case newSample of
                Nothing -> do
                    modifyState (\s -> s { _activeSamples = heap' })
                    modifyState (\s -> s { _result = closestActiveSample : _result s })
                Just sample -> addSample sample

            sampleLoop

findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = foldr (\x xs -> p x >>= \u -> if u then pure (Just x) else xs) (pure Nothing)

-- | http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
nextCandidates :: PrimMonad m => Vec2 -> PoissonT m [Vec2]
nextCandidates v = do
    PoissonDiscState{..} <- getsState id
    PoissonDiscParams{..} <- asksParams id
    phi0 <- lift (rad <$> uniformRM (0, 2*pi) _gen)
    let deltaPhi = rad (2*pi / fromIntegral _poissonK)
        candidates = filter (isWithinBounds _poissonWidth _poissonHeight)
            [ v +. polar (phi0 +. i *. deltaPhi) r
            | let r = _poissonRadius + 0.000001
            , i <- [1..fromIntegral _poissonK] ]
    pure candidates

  where
    isWithinBounds w h (Vec2 x y) = x >= 0 && x <= fromIntegral w && y >= 0 && y <= fromIntegral h

addSample :: Monad m => Vec2 -> PoissonT m ()
addSample sample = do
    cell <- gridCell sample
    modifyState (\s -> s { _grid = M.insert cell sample (_grid s)})
    distanceFromInitial <- do
        initial <- getsState _initialPoint
        pure (norm (sample -. initial))
    modifyState (\s -> s { _activeSamples = H.insert (H.Entry distanceFromInitial sample) (_activeSamples s) })

-- A cell in the grid has a side length of r/sqrt(2). Therefore, to detect
-- collisions, we need to search a space of 5x5 cells at max.
neighbouringSamples :: Monad m => Vec2 -> PoissonT m [Vec2]
neighbouringSamples v = do
    (x, y) <- gridCell v
    let minX = x - 2
        maxX = x + 2
        minY = y - 2
        maxY = y + 2
    neighbours <- sequence $ do
        cellX <- [minX..maxX]
        cellY <- [minY..maxY]
        pure (maybeToList . M.lookup (cellX, cellY) <$> getsState _grid)
    pure (concat neighbours)

gridCell :: Monad m => Vec2 -> PoissonT m (Int, Int)
gridCell (Vec2 x y) = do
    r <- asksParams _poissonRadius
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

-- | @'gaussianDistributedPoints' gen mu sigma (width,height) count@
-- generates @count@ normal distributed random points within a rectangle of
-- @width@ \(\times\) @height@, with the given standard deviations.
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
--     'uniformlyDistributedPoints' gen ('Vec2' 0 0, 'Vec2' 64 64) ('Mat2' 10 0 0 10) 100
-- @
gaussianDistributedPoints
    :: (PrimMonad m, HasBoundingBox boundingBox)
    => Gen (PrimState m) -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> boundingBox       -- ^ Determines width and height. The center of this is the mean \(\mathbf\mu\).
    -> Mat2              -- ^ Covariance matrix \(\mathbf\Sigma\).
    -> Int               -- ^ Number of points.
    -> m (Vector Vec2)
gaussianDistributedPoints gen container covariance count = V.replicateM count randomPoint
  where
    bb = boundingBox container
    center = boundingBoxCenter bb

    randomPoint = do
        let t = Transformation covariance center
        vec <- transform t . Vec2 <$> standard gen <*> standard gen
        if vec `insideBoundingBox` bb
            then pure vec
            else randomPoint
