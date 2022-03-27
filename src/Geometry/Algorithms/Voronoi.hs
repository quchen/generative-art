-- | A Voronoi pattern paints a cell around each point, so that each point in the
-- cell is closest to that point.
--
-- >>> :{
-- haddockRender "Geometry/Algorithms/Voronoi.hs/voronoi.svg" 400 300 $ do
--     points <- liftIO $ do
--         gen <- MWC.create
--         gaussianDistributedPoints gen [zero, Vec2 400 300] (Mat2 200 0 0 150) 16
--     let voronoi = mkVoronoi 400 300 (fmap (\point -> (point, ())) points)
--     for_ (_voronoiCells voronoi) $ \cell -> do
--         sketch (_voronoiRegion cell)
--         stroke
--         sketch (Circle (_voronoiSeed cell) 2)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Voronoi.hs/voronoi.svg
--
-- <<docs/haddock/Geometry/Algorithms/Voronoi.hs/voronoi.svg>>
module Geometry.Algorithms.Voronoi (
    -- * Types
    --
    -- A Voronoi pattern is constructed from a list of seeds:
    -- Each seed is surrounded by a polygon so that the distance of all
    -- points in the polygon is closer to the seed than to any other
    -- seed in the plane.
    --
    -- The phyiscal analogon is crystallization around a nucleus: Starting from a
    -- nucleus, the crystal grows in every direction, until it hits the crystal
    -- structure of another nucleus.

    Voronoi(..)
    , VoronoiCell(..)
    , mapWithMetadata
    , mapWithSeed
    , mapWithRegion

    -- * Construction
    , emptyVoronoi
    , mkVoronoi
    , addPoint

    -- * Internal
    , updateCell
) where



import Data.List (foldl')

import Geometry

-- $setup
-- >>> import Draw
-- >>> import Graphics.Rendering.Cairo
-- >>> import Geometry.Algorithms.Sampling
-- >>> import qualified System.Random.MWC as MWC



data VoronoiCell a = VoronoiCell
    { _voronoiSeed :: Vec2      -- ^ The point around which the cell grows.
    , _voronoiRegion :: Polygon -- ^ The cell itself.
    , _voronoiProps :: a        -- ^ Any additional data, e.g. the color of the cell.
    } deriving (Eq, Show)

-- | A Voronoi diagram, with a possible tag for each cell, e.g. for coloring.
--
-- Voronoi patterns are constructed using 'mkVoronoi' or 'addPoint'.
data Voronoi a = Voronoi
    { _voronoiBounds :: BoundingBox
    -- ^ The bounding box. Also used as a basis for all newly inserted polygons.
    , _voronoiCells :: [VoronoiCell a]
    -- ^ A list of Voronoi cells. Don't add any cells yourself, use 'mkVoronoi'
    -- or 'addPoint' instead.
    }
    deriving (Eq, Show)

instance Functor VoronoiCell where
    fmap f cell@VoronoiCell{..} = cell { _voronoiProps = f _voronoiProps }

instance Functor Voronoi where
    fmap f voronoi@Voronoi{..} = voronoi { _voronoiCells = fmap (fmap f) _voronoiCells }

-- | Rewrite the tags of every cell, taking the position of the seed and the region into account.
mapWithMetadata :: (Vec2 -> Polygon -> a -> b) -> Voronoi a -> Voronoi b
mapWithMetadata f voronoi@Voronoi{..} = voronoi { _voronoiCells = [ cell { _voronoiProps = f _voronoiSeed _voronoiRegion _voronoiProps } | cell@VoronoiCell{..} <- _voronoiCells ] }

{-# DEPRECATED mapWithRegion "Use mapWithMetadata instead" #-}
mapWithRegion :: (Polygon -> a -> b) -> Voronoi a -> Voronoi b
mapWithRegion f voronoi@Voronoi{..} = voronoi { _voronoiCells = [ cell { _voronoiProps = f _voronoiRegion _voronoiProps } | cell@VoronoiCell{..} <- _voronoiCells ] }

{-# DEPRECATED mapWithSeed "Use mapWithMetadata instead" #-}
mapWithSeed :: (Vec2 -> a -> b) -> Voronoi a -> Voronoi b
mapWithSeed f voronoi@Voronoi{..} = voronoi { _voronoiCells = [ cell { _voronoiProps = f _voronoiSeed _voronoiProps } | cell@VoronoiCell{..} <- _voronoiCells ] }

-- | Construct a Voronoi pattern from a list of tagged seeds.
--
-- 'mkVoronoi' constructs a Voronoi pattern by iteratively adding points.
--
-- Basically, @mkVoronoi w h = foldl' 'addPoint' ('emptyVoronoi' w h)
mkVoronoi :: Foldable f => Double -> Double -> f (Vec2, a) -> Voronoi a
mkVoronoi w h = foldl' addPoint (emptyVoronoi w h)

-- | The starting point for a Voronoi pattern.
emptyVoronoi :: Double -> Double -> Voronoi a
emptyVoronoi w h = Voronoi (boundingBox [zero, Vec2 w h]) []

-- | Add a new seed point to a Voronoi pattern.
--
-- The algorithm works as follows:
--
--     * For all existing cells, remove the portion that is nearer to the new
--       seed than to the seed of the cell.
--     * For the new cell, start with the largest possible area ('bounds'), and
--       iterate over all existing seeds and remove the portion that is
--       nearer to the other seed.
--
-- The new cell is then inserted, and together with the clipped existing cells
-- it will cover the area exactly without gaps or overlap.
--
-- Note that this algorithm is pretty simple, but also inefficient, since most
-- of the existing cells don't have any overlap with the new cell, so we do a
-- a lot of unnecessary checks. The algorithm runs in O(n²) time.
addPoint :: Voronoi a -> (Vec2, a) -> Voronoi a
addPoint Voronoi{..} (p, a) = Voronoi _voronoiBounds (newCell : cells')
  where
    newCell = foldl' (\nf f -> updateCell (_voronoiSeed f) nf) (VoronoiCell p (boundingBoxPolygon _voronoiBounds) a) _voronoiCells
    cells' = fmap (updateCell (_voronoiSeed newCell)) _voronoiCells

-- | The heart of 'addPoint': Given a seed and a 'VoronoiCell', remove
-- everything from the cell that is nearer to the new seed than to the
-- seed of the original cell.
updateCell :: Vec2 -> VoronoiCell a -> VoronoiCell a
updateCell p f = clipCell (perpendicularBisector (Line (_voronoiSeed f) p)) f

clipCell :: Line -> VoronoiCell a -> VoronoiCell a
clipCell line f =
    case filter (pointInPolygon (_voronoiSeed f)) (cutPolygon line (_voronoiRegion f)) of
        [p] -> f { _voronoiRegion = p }
        [] -> bugError "Could not identify the remaining Voronoi cell. Perhaps the seed was outside the cell to start with?"
        _ -> bugError "`cutPolygon` resulted in overlapping polygons."
