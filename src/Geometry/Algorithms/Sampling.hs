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
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           System.Random.MWC
import           System.Random.MWC.Distributions

import Geometry
import Geometry.Algorithms.Sampling.PoissonDisc



-- | @'uniformlyDistributedPoints' gen width height count@ generates @count@
-- random points within a rectangle of @width@ x @height@.
--
-- Create 100 points in a 64*64 square:
--
-- @
-- points :: 'Vector' 'Vec2'
-- points = 'Control.Monad.ST.runST' $ do
--     gen <- 'create'
--     let region = boundingBox [zero, Vec2 64 64]
--     'uniformlyDistributedPoints' gen region 100
-- @
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
