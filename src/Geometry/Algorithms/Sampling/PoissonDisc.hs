module Geometry.Algorithms.Sampling.PoissonDisc (
    poissonDisc
) where



import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State  as S
import           Data.Default.Class
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           System.Random.MWC

import Geometry



-- | Configuration for 'poissonDisc' sampling.
data PoissonDiscParams = PoissonDiscParams
    { _poissonShape  :: !BoundingBox -- ^ 'def'ault @boundingBox [zero, Vec2 256 256]@.
    , _poissonRadius :: !Double -- ^ Minimum distance between points. 'def'ault 100.
    , _poissonK      :: !Int    -- ^ How many attempts to find a neighbouring point should be made?
                                --   The higher this is, the denser the resulting point set will be.
    } deriving (Eq, Ord, Show)

instance Default PoissonDiscParams where
    def = PoissonDiscParams
        { _poissonShape  = boundingBox [zero, Vec2 256 256]
        , _poissonRadius = 100
        , _poissonK      = 32
        }

newtype PoissonT m a = PoissonT {runPoissonT :: R.ReaderT PoissonDiscParams (S.StateT (PoissonDiscState (PrimState m)) m) a}

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
execPoissonT (PoissonT action) params initialState = S.execStateT (R.runReaderT action params) initialState

asks :: Monad m => (PoissonDiscParams -> a) -> PoissonT m a
asks = PoissonT . R.asks

gets :: Monad m => (PoissonDiscState (PrimState m) -> a) -> PoissonT m a
gets = PoissonT . lift . S.gets

modify'
    :: Monad m
    => (PoissonDiscState (PrimState m) -> PoissonDiscState (PrimState m))
    -> PoissonT m ()
modify' = PoissonT . lift . S.modify'

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
--         { _poissonShape = boundingBox [zero, Vec2 80 80]
--         , '_poissonRadius' = 8
--         , '_poissonK'      = 4
--         }
-- @
poissonDisc
    :: (PrimMonad m, HasBoundingBox boundingBox)
    => Gen (PrimState m) -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> boundingBox -- ^ Region to generate points in
    -> Double      -- ^ Radius around each point no other points are genereted. Smaller values yield more points.
    -> Int         -- ^ Per point, how many attempts should be made to find an empty spot?
                   --   Typical value: 3. Higher values are slower, but increase result quality.
    -> m [Vec2]
poissonDisc gen bb' radius k = do
    let bb@(BoundingBox minV maxV) = boundingBox bb'

    initialSample <- uniformRM (minV, maxV) gen
    let initialState = PoissonDiscState
            { _gen           = gen
            , _grid          = mempty
            , _activeSamples = mempty
            , _result        = mempty
            , _initialPoint  = initialSample
            }

    _result <$> execPoissonT (addSample initialSample >> sampleLoop) (PoissonDiscParams bb radius k) initialState

data PoissonDiscState s = PoissonDiscState
    { _gen           :: !(Gen s)
    , _grid          :: !(Map (Int, Int) Vec2)
    , _activeSamples :: !(Set Vec2)
    , _result        :: ![Vec2]
    , _initialPoint  :: !Vec2
    }

sampleLoop :: PrimMonad m => PoissonT m ()
sampleLoop = do
    numActiveSamples <- gets (S.size . _activeSamples)
    when (numActiveSamples > 0) $ do
        randomSampleIndex <- gets _gen >>= lift . uniformRM (0, numActiveSamples - 1)
        randomActiveSample <- gets (S.elemAt randomSampleIndex . _activeSamples)

        r <- asks _poissonRadius

        candidates <- nextCandidates randomActiveSample

        let validPoint candidate = do
                neighbours <- neighbouringSamples candidate
                pure (not (any (\p -> norm (candidate -. p) <= r) neighbours))

        newSample <- findM validPoint candidates

        case newSample of
            Nothing -> modify' (\s -> s
                { _activeSamples = S.delete randomActiveSample (_activeSamples s)
                , _result = randomActiveSample : _result s })
            Just sample -> addSample sample

        sampleLoop

findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = foldr (\x xs -> p x >>= \u -> if u then pure (Just x) else xs) (pure Nothing)

-- | http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
nextCandidates :: PrimMonad m => Vec2 -> PoissonT m [Vec2]
nextCandidates v = do
    PoissonDiscState{..} <- gets id
    PoissonDiscParams{..} <- asks id
    phi0 <- lift (rad <$> uniformRM (0, 2*pi) _gen)
    let deltaPhi = rad (2*pi / fromIntegral _poissonK)
        candidates = filter (`insideBoundingBox` _poissonShape)
            [ v +. polar (phi0 +. i *. deltaPhi) r
            | let r = _poissonRadius + 0.000001
            , i <- [1..fromIntegral _poissonK] ]
    pure candidates

addSample :: Monad m => Vec2 -> PoissonT m ()
addSample sample = do
    cell <- gridCell sample
    modify' (\s -> s
        { _grid = M.insert cell sample (_grid s)
        , _activeSamples = S.insert sample (_activeSamples s) })

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
        pure (maybeToList . M.lookup (cellX, cellY) <$> gets _grid)
    pure (concat neighbours)

gridCell :: Monad m => Vec2 -> PoissonT m (Int, Int)
gridCell (Vec2 x y) = do
    r <- asks _poissonRadius
    let gridSize = r / sqrt 2
    pure (floor (x/gridSize), floor (y/gridSize))
