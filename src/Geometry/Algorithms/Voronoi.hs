-- | A Voronoi pattern is constructed from a list of seeds:
-- Each seed is surrounded by a polygon so that the distance of all
-- points in the polygon is closer to the seed than to any other
-- seed in the plane.
--
-- The phyiscal analogon is crystallization around a nucleus: Starting from a
-- nucleus, the crystal grows in every direction, until it hits the crystal
-- structure of another nucleus.
--
-- >>> :{
-- haddockRender "Geometry/Algorithms/Voronoi.hs/voronoi.svg" 300 200 $ do
--     points <- liftIO $ do
--         gen <- MWC.create
--         gaussianDistributedPoints gen [zero, Vec2 300 200] (Mat2 150 0 0 100) 16
--     let voronoi = mkVoronoi [zero, Vec2 300 200] (fmap (\point -> (point, ())) points)
--     for_ (_voronoiCells voronoi) $ \cell -> do
--         sketch (_voronoiRegion cell)
--         setColor (mathematica97 0)
--         stroke
--         sketch (Circle (_voronoiSeed cell) 3)
--         setColor (mathematica97 1)
--         fill
-- :}
-- docs/haddock/Geometry/Algorithms/Voronoi.hs/voronoi.svg
--
-- <<docs/haddock/Geometry/Algorithms/Voronoi.hs/voronoi.svg>>
module Geometry.Algorithms.Voronoi (
    -- * Types


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
-- Voronoi patterns are constructed using 'mkVoronoi' or 'addPoint'/'emptyVoronoi'.
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

{-# DEPRECATED mapWithRegion, mapWithSeed "Use mapWithMetadata instead" #-}
mapWithRegion :: (Polygon -> a -> b) -> Voronoi a -> Voronoi b
mapWithRegion f = mapWithMetadata (\_ polygon meta -> f polygon meta)
mapWithSeed :: (Vec2 -> a -> b) -> Voronoi a -> Voronoi b
mapWithSeed f = mapWithMetadata (\polygon _ meta -> f polygon meta)

-- | Construct a Voronoi pattern from a list of tagged seeds.
--
-- 'mkVoronoi' constructs a Voronoi pattern by iteratively adding points.
mkVoronoi
    :: (Foldable f, HasBoundingBox region)
    => region -- ^ Defines the boundary of the pattern
    -> f (Vec2, a)
    -> Voronoi a
mkVoronoi world = foldl' addPoint (emptyVoronoi world)

-- | The starting point for a Voronoi pattern.
emptyVoronoi :: HasBoundingBox region => region -> Voronoi a
emptyVoronoi world = Voronoi (boundingBox world) []

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

-- | Remove everything from a cell that’s beyond the specified line.
clipCell :: Line -> VoronoiCell a -> VoronoiCell a
clipCell line f =
    case filter (pointInPolygon (_voronoiSeed f)) (cutPolygon line (_voronoiRegion f)) of
        [p] -> f { _voronoiRegion = p }
        [] -> bugError "Could not identify the remaining Voronoi cell. Perhaps the seed was outside the cell to start with?"
        _ -> bugError "`cutPolygon` resulted in overlapping polygons."
