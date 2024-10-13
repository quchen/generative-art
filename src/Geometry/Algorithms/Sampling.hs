module Geometry.Algorithms.Sampling (
    -- * Poisson-Disc sampling
      poissonDisc

    -- * Other distributions
    , uniformlyDistributedPoints
    , gaussianDistributedPoints
    , rejection
) where



import           Control.Applicative             (liftA2)
import           Control.Monad.Primitive
import           Data.Function
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           System.Random.MWC
import           System.Random.MWC.Distributions

import Geometry
import Geometry.Algorithms.Sampling.PoissonDisc


-- $setup
-- >>> import qualified System.Random.MWC         as MWC
-- >>> import           Graphics.Rendering.Cairo
-- >>> import           Draw
-- >>> import           Numerics.Functions
-- >>> import           Control.Monad.ST



-- | Generate uniformly distributed points.
--
-- <<docs/haddock/Geometry/Algorithms/Sampling/uniform.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Sampling/uniform.svg" 300 300 $ \_ -> do
--     let numPoints = 1000
--         bb = shrinkBoundingBox 30 [zero, Vec2 300 300]
--         points = runST $ do
--             gen <- MWC.create
--             uniformlyDistributedPoints gen bb numPoints
--     V.iforM_ points $ \i p -> do
--         setColor (mma i)
--         sketch (Circle p 2)
--         fill
-- :}
-- Generated file: size 263KB, crc32: 0x29e19f0e
uniformlyDistributedPoints
    :: (PrimMonad m, HasBoundingBox boundingBox)
    => Gen (PrimState m) -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> boundingBox       -- ^ Region to generate points in
    -> Int               -- ^ Number of points
    -> m (Vector Vec2)
uniformlyDistributedPoints gen bb count = V.replicateM count randomPoint
  where
    BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax) = boundingBox bb
    randomPoint = liftA2 Vec2 (uniformR (xMin, xMax) gen) (uniformR (yMin, yMax) gen)

-- | Generate Gaussian/normal distributed points.
--
-- Note: This is a rejection algorithm which discards samples outside of the
-- 'BoundingBox'. If you choose the covariance much larger than the height or
-- width, performance will deteriorate as more and more points are rejected.
--
-- <<docs/haddock/Geometry/Algorithms/Sampling/gaussian.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Sampling/gaussian.svg" 300 300 $ \_ -> do
--     let numPoints = 1000
--         bb = shrinkBoundingBox 30 [zero, Vec2 300 300]
--         points = runST $ do
--             gen <- MWC.create
--             gaussianDistributedPoints gen bb (30 *. mempty) numPoints
--     V.iforM_ points $ \i p -> do
--         setColor (mma i)
--         sketch (Circle p 2)
--         fill
-- :}
-- Generated file: size 267KB, crc32: 0xdffc4138
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

-- | Sample a distribution via rejection sampling: pick a random coordinate, check
-- whether the distribution’s value exceeds that value (accept the sample) or
-- retry.
--
-- The distribution does not need to be normalized, but its values should be
-- reasonably close to 1 for part of the domain, or the algorithm will take a long
-- time to sample the points.
--
-- <<docs/haddock/Geometry/Algorithms/Sampling/rejection.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Sampling/rejection.svg" 300 300 $ \_ -> do
--     let numPoints = 1000
--         bb = shrinkBoundingBox 30 [zero, Vec2 300 300]
--         distribution p = let r = norm (p -. Vec2 150 150) in gaussianFalloff 75 20 r
--         points = runST $ do
--             gen <- MWC.create
--             rejection gen bb distribution numPoints
--     V.iforM_ points $ \i p -> do
--         setColor (mma i)
--         sketch (Circle p 2)
--         fill
-- :}
-- Generated file: size 263KB, crc32: 0x38511a1
rejection
    :: (PrimMonad m, HasBoundingBox boundingBox)
    => Gen (PrimState m) -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> boundingBox       -- ^ Area to sample in
    -> (Vec2 -> Double)  -- ^ Distribution \(w(\mathbf p) \in [0\ldots 1]\).
    -> Int               -- ^ Number of points
    -> m (Vector Vec2)
rejection gen bb distribution numPoints = V.generateM numPoints $ \_ -> sampleSinglePoint
  where
    sampleSinglePoint = fix $ \loop -> do
        p <- uniformRM (bbMin, bbMax) gen
        let pVal = distribution p
        threshold <- uniformRM (0,1) gen
        if threshold < pVal
            then pure p
            else loop

    BoundingBox bbMin bbMax = boundingBox bb
