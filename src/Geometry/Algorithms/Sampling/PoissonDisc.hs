-- | Fast, high-quality Poisson disc sampling, based on [Bridson’s algorithm](https://dl.acm.org/doi/10.1145/1278780.1278807),
-- then [improved by Martin Robers](https://observablehq.com/@techsparx/an-improvement-on-bridsons-algorithm-for-poisson-disc-samp/2),
-- then improved an \(\varepsilon\) bit more here (4 cells in 'neighbouringPoints' can be skipped).
module Geometry.Algorithms.Sampling.PoissonDisc (
      poissonDisc
    , poissonDiscForest
) where



import           Control.Monad.Primitive
import           Data.List
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Data.Vector             (Vector)
import qualified Data.Vector.Extended    as V
import           System.Random.MWC

import Geometry



-- $setup
-- >>> import           Draw
-- >>> import           Control.Monad.ST
-- >>> import           Numerics.Interpolation
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
    -> Int         -- ^ \(k\) parameter: per point, how many attempts should be made to find an empty spot?
                   --   Typical values: 3-10. Higher values are slower, but increase result quality.
    -> m [Vec2]
poissonDisc gen bb' radius k = do
    let bb@(BoundingBox minV maxV) = boundingBox bb'
        cellSize = CellSize (radius/sqrt 2)
    initialPoint <- uniformRM (minV, maxV) gen
    let initialGrid = M.singleton (gridCell cellSize initialPoint) initialPoint
        initialActive = S.singleton initialPoint

        sampleLoop _grid active
            | S.null active = pure []
        sampleLoop grid active = do
            activeSample <- randomSetElement gen active
            candidates <- candidatesAroundSample gen k bb radius activeSample

            let validPoint candidate =
                    let neighbours = neighbouringPoints cellSize grid candidate
                        tooClose neighbour = normSquare (candidate -. neighbour) <= radius^2
                    in not (any tooClose neighbours)

            case find validPoint candidates of
                Nothing -> do
                    rest <- sampleLoop grid (S.delete activeSample active)
                    pure (activeSample : rest)
                Just sample -> sampleLoop
                    (M.insert (gridCell cellSize sample) sample grid)
                    (S.insert sample active)

    sampleLoop initialGrid initialActive

-- | 'poissonDisc', but keeps track of which parent spawned which children. While
-- this algorithm does a bit more processing to reassemble the trees, it allows
-- specifying the starting points explicitly. ('poissonDisc' would also allow this,
-- but since it’s mostly used for simply sampling, this additional config option
-- would just clutter the API.)
--
-- <<docs/haddock/Geometry/Algorithms/Sampling/PoissonDisc/poisson_disc_forest.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Algorithms/Sampling/PoissonDisc/poisson_disc_forest.svg" 300 300 $ do
--     let initialPoints = [Vec2 50 50, Vec2 150 150, Vec2 250 250]
--         forest = runST $ do
--             gen <- MWC.create
--             poissonDiscForest gen (shrinkBoundingBox 30 [zero, Vec2 300 300]) 7 10 initialPoints
--     let paint color i parent = do
--             let parentRadius = 2.5
--             cairoScope $ do
--                 setColor (color (sin (lerpID (0,20) (0,pi) i)^2))
--                 sketch (Circle parent parentRadius)
--                 fill
--             case M.lookup parent forest of
--                 Nothing -> pure () -- Should never happen: only the roots have no parents
--                 Just children -> for_ children $ \child -> do
--                     cairoScope $ do
--                         setColor black
--                         sketch (resizeLineSymmetric (\l -> l-2*parentRadius) (Line parent child))
--                         stroke
--                     paint color (i+1) child
--     for_ (zip [flare, crest, flare] initialPoints) $ \(color, p0) -> paint color 0 p0
-- :}
-- docs/haddock/Geometry/Algorithms/Sampling/PoissonDisc/poisson_disc_forest.svg
poissonDiscForest
    :: (PrimMonad m, HasBoundingBox boundingBox)
    => Gen (PrimState m)       -- ^ RNG from mwc-random. 'create' yields the default (static) RNG.
    -> boundingBox             -- ^ Region to generate points in
    -> Double                  -- ^ Radius around each point no other points are genereted. Smaller values yield more points.
    -> Int                     -- ^ \(k\) parameter: per point, how many attempts should be made to find an empty spot?
                               --   Typical values: 3-10. Higher values are slower, but increase result quality.
    -> [Vec2]                  -- ^ Initial points to start sampling from.
    -> m (Map Vec2 (Set Vec2)) -- ^ Map of parent to children.
poissonDiscForest gen bb' radius k initialPoints = do
    let bb = boundingBox bb'
        cellSize = CellSize (radius/sqrt 2)
    let initialGrid = M.fromList [(gridCell cellSize initialPoint, initialPoint) | initialPoint <- initialPoints]
        initialActive = S.fromList [ChildParent initialPoint Nothing | initialPoint <- initialPoints]

        sampleLoop _grid active
            | S.null active = pure []
        sampleLoop grid active = do
            ChildParent activeSample parent <- randomSetElement gen active
            candidates <- candidatesAroundSample gen k bb radius activeSample

            let validPoint candidate =
                    let neighbours = neighbouringPoints cellSize grid candidate
                        tooClose neighbour = normSquare (candidate -. neighbour) <= radius^2
                    in not (any tooClose neighbours)

            case find validPoint candidates of
                Nothing -> do
                    rest <- sampleLoop grid (S.delete (ChildParent activeSample parent) active)
                    pure (ChildParent activeSample parent : rest)
                Just sample -> sampleLoop
                    (M.insert (gridCell cellSize sample) sample grid)
                    (S.insert (ChildParent sample (Just activeSample)) active)

    edges <- sampleLoop initialGrid initialActive
    let families = mapMaybe
            (\(ChildParent child maybeParent) -> case maybeParent of
                Nothing -> Nothing
                Just parent -> Just (parent, S.singleton child))
            edges
    pure (M.fromListWith S.union families)

data ChildParent = ChildParent Vec2 (Maybe Vec2)

instance Eq ChildParent where
    ChildParent child1 _parent1 == ChildParent child2 _parent2 = child1 == child2

instance Ord ChildParent where
    ChildParent child1 _parent1 `compare` ChildParent child2 _parent2 = child1 `compare` child2

randomSetElement :: PrimMonad m => Gen (PrimState m) -> Set a -> m a
randomSetElement gen set = do
    i <- uniformRM (0, S.size set-1) gen
    pure (S.elemAt i set)

-- | http://extremelearning.com.au/an-improved-version-of-bridsons-algorithm-n-for-poisson-disc-sampling/
--
-- Enhanced by a random trial order so we don’t privilege clockwise selection
candidatesAroundSample
    :: PrimMonad m
    => Gen (PrimState m)
    -> Int -- ^ Number of attempts
    -> BoundingBox -- ^ Sampling region
    -> Double -- ^ Poisson radius
    -> Vec2 -- ^ Point to generate candidates around
    -> m (Vector Vec2)
candidatesAroundSample gen k bb r v = do
    phi0 <- rad <$> uniformRM (0, 2*pi) gen
    let deltaPhi = rad (2*pi / fromIntegral k)
        circle = V.generate k (\i -> v +. polar (phi0 +. fromIntegral i *. deltaPhi) (r + 1e-6))
        inside = V.filter (`insideBoundingBox` bb) circle
    inside' <- V.thaw inside
    V.fisherYatesShuffle gen inside'
    V.freeze inside'

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
