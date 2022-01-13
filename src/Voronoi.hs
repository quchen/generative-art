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

-- * Construction
, emptyVoronoi
, mkVoronoi
, mkVoronoi'
, addPoint
, addPoint'

-- * Randomness
, uniformlyDistributedPoints
, gaussianDistributedPoints

-- * Internal
, updateCell
) where

import Data.List                       (foldl')
import System.Random.MWC               (GenIO, uniformR)
import System.Random.MWC.Distributions (normal)

import Control.Applicative (liftA2)
import Control.Monad       (replicateM)
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
    { bounds :: Polygon
    -- ^ The bounding box. Also used as a basis for all newly inserted polygons.
    , cells :: [VoronoiCell a]
    -- ^ A list of Voronoi cells. Don't add any cells yourself, use 'mkVoronoi'
    -- or 'addPoint' instead.
    }
    deriving (Eq, Show)

type Voronoi' = Voronoi ()

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
emptyVoronoi w h = Voronoi initialPolygon []
  where
    initialPolygon = Polygon [ Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h ]

-- | @'uniformlyDistributedPoints' gen width height count@ generates @count@
-- random points within a rectangle of @width@ x @height@.
uniformlyDistributedPoints :: GenIO -> Int -> Int -> Int -> IO [Vec2]
uniformlyDistributedPoints gen width height count = replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width) (randomCoordinate height)
    randomCoordinate mx = fmap fromIntegral (uniformR (0, mx) gen :: IO Int)

-- | @'uniformlyDistributedPoints' gen (width, sigmaX) (height, sigmaY) count@
-- generates @count@ normal distributed random points within a rectangle of
-- @width@ x @height@, with a the given standard deviations.
--
-- Note: This is a rejection algorithm. If you choose the standard deviation
-- much higher than the height or width, performance will deteriorate as more
-- and more points are rejected.
gaussianDistributedPoints :: GenIO -> (Int, Double) -> (Int, Double) -> Int -> IO [Vec2]
gaussianDistributedPoints gen (width, sigmaX) (height, sigmaY) count = replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width sigmaX) (randomCoordinate height sigmaY)
    randomCoordinate mx sigma = do
        coord <- normal (fromIntegral mx/2) sigma gen :: IO Double
        if coord < 0 || coord > fromIntegral mx
            then randomCoordinate mx sigma
            else pure coord

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
    newCell = foldl' (\nf f -> updateCell (seed f) nf) (Cell p bounds a) cells
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
