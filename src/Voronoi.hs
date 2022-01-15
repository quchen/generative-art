{-# LANGUAGE RecordWildCards #-}
module Voronoi
(
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
, Voronoi'
, VoronoiCell(..)
, mapWithSeed

-- * Construction
, emptyVoronoi
, mkVoronoi
, mkVoronoi'
, addPoint
, addPoint'

-- * Internal
, updateCell
) where

import Data.List                       (foldl')

import Geometry

data VoronoiCell a = Cell
    { seed :: Vec2
    -- ^ The point around which the cell grows.
    , region :: Polygon
    -- ^ The cell itself.
    , props :: a
    -- ^ Any additional data, e.g. the color of the cell.
    }
    deriving (Eq, Show)

-- | Voronoi patterns should be constructed using 'mkVoronoi' or 'addPoint'.
data Voronoi a = Voronoi
    { bounds :: BoundingBox
    -- ^ The bounding box. Also used as a basis for all newly inserted polygons.
    , cells :: [VoronoiCell a]
    -- ^ A list of Voronoi cells. Don't add any cells yourself, use 'mkVoronoi'
    -- or 'addPoint' instead.
    }
    deriving (Eq, Show)

type Voronoi' = Voronoi ()

instance Functor VoronoiCell where
    fmap f cell@Cell{..} = cell { props = f props }

instance Functor Voronoi where
    fmap f voronoi@Voronoi{..} = voronoi { cells = fmap (fmap f) cells }

-- | Rewrite the tags of every cell, taking the position of the seed into account.
mapWithSeed :: (Vec2 -> a -> b) -> Voronoi a -> Voronoi b
mapWithSeed f voronoi@Voronoi{..} = voronoi { cells = [ cell { props = f seed props } | cell@Cell{..} <- cells ] }

-- | Construct a Voronoi pattern from a list of tagged seeds.
--
-- 'mkVoronoi' constructs a Voronoi pattern by iteratively adding points.
--
-- Basically, @mkVoronoi w h = foldl' 'addPoint' ('emptyVoronoi' w h)
mkVoronoi :: Double -> Double -> [(Vec2, a)] -> Voronoi a
mkVoronoi w h = foldl' addPoint (emptyVoronoi w h)

-- | Same as 'mkVoronoi', but omitting the 'props'.
mkVoronoi' :: Double -> Double -> [Vec2] -> Voronoi'
mkVoronoi' w h = foldl' addPoint' (emptyVoronoi w h)

-- | The starting point for a Voronoi pattern.
emptyVoronoi :: Double -> Double -> Voronoi a
emptyVoronoi w h = Voronoi (BoundingBox (Vec2 0 0) (Vec2 w h)) []

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
addPoint Voronoi{..} (p, a) = Voronoi bounds (newCell : cells')
  where
    newCell = foldl' (\nf f -> updateCell (seed f) nf) (Cell p (boundingBoxPolygon bounds) a) cells
    cells' = fmap (updateCell (seed newCell)) cells

-- | Same as 'addPoint', but without 'props'.
addPoint' :: Voronoi' -> Vec2 -> Voronoi'
addPoint' voronoi point = addPoint voronoi (point, ())

-- | The heart of 'addPoint': Given a seed and a 'VoronoiCell', remove
-- everything from the cell that is nearer to the new seed than to the
-- seed of the original cell.
updateCell :: Vec2 -> VoronoiCell a -> VoronoiCell a
updateCell p f = clipCell (perpendicularBisector (Line (seed f) p)) f

clipCell :: Line -> VoronoiCell a -> VoronoiCell a
clipCell line f =
    case filter (pointInPolygon (seed f)) (cutPolygon line (region f)) of
        [p] -> f { region = p }
        [] -> bugError "Could not identify the remaining Voronoi cell. Perhaps the seed was outside the cell to start with?"
        _ -> bugError "`cutPolygon` resulted in overlapping polygons."
