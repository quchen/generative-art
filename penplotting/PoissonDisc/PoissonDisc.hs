module PoissonDisc where



import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State  as S
import           Data.Default.Class
import           Data.Heap                  (Entry, Heap)
import qualified Data.Heap                  as H
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           System.Random.MWC

import Geometry



-- | Configuration for 'poissonDisc' sampling.
data PoissonDiscParams = PoissonDiscParams
    { _poissonShape  :: !BoundingBox -- ^ 'def'ault @boundingBox [zero, Vec2 256 256]@.
    , _poissonRadius :: Vec2 -> Double -- ^ Minimum distance of a new point from this point.
    , _poissonK      :: !Int    -- ^ How many attempts to find a neighbouring point should be made?
                                --   The higher this is, the denser the resulting point set will be.
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
    :: PrimMonad m
    => Gen (PrimState m) -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> PoissonDiscParams
    -> m [(Vec2, Vec2, Double)]
poissonDisc gen params = do
    let PoissonDiscParams{_poissonShape = BoundingBox minV maxV, ..} = params
        initialSample = 0.5 *. (minV +. maxV)
        initialState = PoissonDiscState
            { _gen           = gen
            , _allSamples    = mempty
            , _activeSamples = mempty
            , _result        = mempty
            , _initialPoint  = initialSample
            }

    _result <$> execPoissonT (addSample (initialSample, initialSample, _poissonRadius initialSample) >> sampleLoop) params initialState

data PoissonDiscState s = PoissonDiscState
    { _gen           :: !(Gen s)
    , _allSamples    :: !(Heap (Vec2, Double))
    , _activeSamples :: !(Heap (Entry Double (Vec2, Vec2, Double)))
    , _result        :: ![(Vec2, Vec2, Double)]
    , _initialPoint  :: !Vec2
    }

sampleLoop :: PrimMonad m => PoissonT m ()
sampleLoop = gets (H.uncons . _activeSamples) >>= \case
    Nothing -> pure ()
    Just (H.Entry _ (closestActiveSample, parent, radius), heap') -> do

        candidates <- nextCandidates (closestActiveSample, radius)

        let validPoint (candidate, r) = do
                allSamples <- gets _allSamples
                pure (not (any (\(p, r') -> norm (candidate -. p) <= 0.5*(r+r')) allSamples))

        newSample <- findM validPoint candidates

        case newSample of
            Nothing -> modify' (\s -> s
                { _activeSamples = heap'
                , _result = (closestActiveSample, parent, radius) : _result s })
            Just (sample, radius') -> addSample (sample, closestActiveSample, radius')

        sampleLoop

findM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m (Maybe a)
findM p = foldr (\x xs -> p x >>= \u -> if u then pure (Just x) else xs) (pure Nothing)

-- | http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
nextCandidates :: PrimMonad m => (Vec2, Double) -> PoissonT m [(Vec2, Double)]
nextCandidates (v, r) = do
    PoissonDiscState{..} <- gets id
    PoissonDiscParams{..} <- asks id
    candidates <- replicateM _poissonK $ do
        phi <- lift (rad <$> uniformRM (0, 2*pi) _gen)
        r' <- lift (uniformRM (0.5*r, 2*r) _gen)
        let v' = v +. polar phi r'
        pure (v', _poissonRadius v')
    pure (filter ((`insideBoundingBox` _poissonShape) . fst) candidates)

addSample :: Monad m => (Vec2, Vec2, Double) -> PoissonT m ()
addSample (sample, parent, radius) = do
    modify' (\s -> s { _allSamples = H.insert (sample, radius) (_allSamples s) })
    distanceFromInitial <- do
        initial <- gets _initialPoint
        pure (norm (sample -. initial))
    modify' (\s -> s { _activeSamples = H.insert (H.Entry distanceFromInitial (sample, parent, radius)) (_activeSamples s) })
