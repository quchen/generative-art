module Geometry.Algorithms.Sampling.PoissonDisc (
    poissonDisc
) where



import           Control.Monad.Primitive
import           Data.List
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Set                as S
import           System.Random.MWC

import Geometry



-- $setup
-- >>> import           Draw
-- >>> import           Control.Monad.ST
-- >>> import           Graphics.Rendering.Cairo as C
-- >>> import qualified System.Random.MWC        as MWC

-- | Newtype safety wrapper
newtype CellSize = CellSize Double
    deriving (Eq, Ord, Show)

-- | Sample points using the Poisson Disc algorithm, which yields a visually
-- uniform distribution. This is opposed to uniformly distributed points yield
-- clumps and empty areas, which is often undesirable for generative art.
--
-- <<docs/haddock/Geometry/Algorithms/Sampling/PoissonDisc/poisson_disc.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Sampling/PoissonDisc/poisson_disc.svg" 300 300 $ do
--     let points = runST $ do
--             gen <- MWC.create
--             poissonDisc gen (shrinkBoundingBox 30 [zero, Vec2 300 300]) 10 4
--     for_ (zip [0..] points) $ \(i,p) -> do
--         setColor (mathematica97 i)
--         sketch (Circle p 2)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Sampling/PoissonDisc/poisson_disc.svg
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
        cellSize = CellSize (radius/sqrt 2)
    initialPoint <- uniformRM (minV, maxV) gen
    let initialState = PoissonDiscState
            { _grid   = M.singleton (gridCell cellSize initialPoint) initialPoint
            , _active = S.singleton initialPoint
            }

        sampleLoop state
            | S.null (_active state) = pure []
        sampleLoop state = do
            activeSample <- randomSetElement gen (_active state)
            candidates <- candidatesAroundSample gen k bb radius activeSample

            let grid = _grid state
                validPoint candidate =
                    let neighbours = neighbouringPoints cellSize grid candidate
                        tooClose neighbour = normSquare (candidate -. neighbour) <= radius^2
                    in not (any tooClose neighbours)

            case find validPoint candidates of
                Nothing -> do
                    rest <- sampleLoop state
                        { _active = S.delete activeSample (_active state) }
                    pure (activeSample : rest)
                Just sample -> sampleLoop state
                    { _grid = M.insert (gridCell cellSize sample) sample (_grid state)
                    , _active = S.insert sample (_active state) }

    -- TODO remove after refactoring! Old alg was backwards :-)
    fmap reverse $ sampleLoop initialState

data PoissonDiscState = PoissonDiscState
    { _grid   :: Map (Int, Int) Vec2
    , _active :: Set Vec2
    }

randomSetElement :: PrimMonad m => Gen (PrimState m) -> Set a -> m a
randomSetElement gen set = do
    i <- uniformRM (0, S.size set-1) gen
    pure (S.elemAt i set)

-- | http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
candidatesAroundSample
    :: PrimMonad m
    => Gen (PrimState m)
    -> Int -- ^ Number of attempts
    -> BoundingBox -- ^ Sampling region
    -> Double
    -> Vec2
    -> m [Vec2]
candidatesAroundSample gen k shape r v = do
    phi0 <- rad <$> uniformRM (0, 2*pi) gen
    let deltaPhi = rad (2*pi / fromIntegral k)
        candidates = filter (`insideBoundingBox` shape)
            [ v +. polar (phi0 +. fromIntegral i *. deltaPhi) (r + 1e-6)
            | i <- [1..k] ]
    pure candidates

-- A cell in the grid has a side length of r/sqrt(2). If we’re somewhere in the X
-- square and can only move at most a square diagonal, we only need to consider the
-- 21 cells marked with `?`, and X itself.
--
-- +---+---+---+---+---+
-- |   | ? | ? | ? |   |
-- +---+---+---+---+---+
-- | ? | ? | ? | ? | ? |
-- +---+---+---+---+---+
-- | ? | ? | X | ? | ? |
-- +---+---+---+---+---+
-- | ? | ? | ? | ? | ? |
-- +---+---+---+---+---+
-- |   | ? | ? | ? |   |
-- +---+---+---+---+---+
neighbouringPoints :: CellSize -> Map (Int, Int) Vec2 -> Vec2 -> [Vec2]
neighbouringPoints cellSize grid v =
    let (x, y) = gridCell cellSize v
    in mapMaybe (\cell -> M.lookup cell grid)
        [             (x-1, y+2), (x  , y+2), (x+1, y+2)
        , (x-2, y+1), (x-1, y+1), (x  , y+1), (x+1, y+1), (x+2, y+1)
        , (x-2, y  ), (x-1, y  ), (x  , y  ), (x+1, y  ), (x+2, y  )
        , (x-2, y-1), (x-1, y-1), (x  , y-1), (x+1, y-1), (x+2, y-1)
        ,             (x-1, y-2), (x  , y-2), (x+1, y-2)
        ]

gridCell :: CellSize -> Vec2 -> (Int, Int)
gridCell (CellSize cellSize) (Vec2 x y) = (floor (x/cellSize), floor (y/cellSize))
{-# INLINE gridCell #-}
