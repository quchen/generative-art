{-# LANGUAGE RecordWildCards #-}
module Voronoi
(
-- * Types
--
-- A Voronoi pattern is constructed from a list of center points or nuclei:
-- Each center point is surrounded by a polygon so that the distance of all
-- points in the polygon is closer to the center point than to any other
-- center point in the plane.
--
-- The phyiscal analogon is crystallization around a nucleus: Starting from a
-- nucleus, the crystal grows in every direction, until it hits the crystal
-- structure of another nucleus.

  Voronoi(..)
, Voronoi'
, VoronoiFace(..)

-- * Construction
, emptyVoronoi
, mkVoronoi
, mkVoronoi'
, addPoint
, addPoint'

-- Internal
, updateFace
) where

import           Data.List (foldl')

import           Geometry

data VoronoiFace a = VF
    { center :: Vec2
    -- ^ The point around which the face grows.
    , face :: Polygon
    -- ^ The face itself.
    , props :: a
    -- ^ Any additional data, e.g. the color of the face.
    }
    deriving (Eq, Show)

-- | Voronoi patterns should be constructed using 'mkVoronoi' or 'addPoint'.
data Voronoi a = Voronoi
    { bounds :: Polygon
    -- ^ The bounding box. Also used as a basis for all newly inserted polygons.
    , faces :: [VoronoiFace a]
    -- ^ A list of Voronoi faces. Don't add any faces yourself, use 'mkVoronoi'
    -- or 'addPoint' instead.
    }
    deriving (Eq, Show)

type Voronoi' = Voronoi ()

-- | Construct a Voronoi pattern from a list of tagged center points.
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

-- | Add a new center point or nucleus to a Voronoi pattern.
--
-- The algorithm works as follows:
--
--     * For all existing faces, remove the portion that is nearer to the new
--       center point than to the center point of the face.
--     * For the new face, start with the largest possible area ('bounds'), and
--       iterate over all existing center points and remove the portion that is
--       nearer to the other center point.
--
-- The new face is then inserted, and together with the clipped existing faces
-- it will cover the area exactly without gaps or overlap.
--
-- Note that this algorithm is pretty simple, but also inefficient, since most
-- of the existing faces don't have any overlap with the new face, so we do a
-- a lot of unnecessary checks.
addPoint :: Voronoi a -> (Vec2, a) -> Voronoi a
addPoint Voronoi{..} (p, a) = Voronoi bounds (newFace : faces')
  where
    newFace = foldl' (\nf f -> updateFace (center f) nf) (VF p bounds a) faces
    faces' = fmap (updateFace (center newFace)) faces

-- | Same as 'addPoint', but without 'props'.
addPoint' :: Voronoi' -> Vec2 -> Voronoi'
addPoint' voronoi point = addPoint voronoi (point, ())

-- | The heart of 'addPoint': Given a center point and a 'VoronoiFace', remove
-- everything from the face that is nearer to the new center point than to the
-- center point of the original face.
updateFace :: Vec2 -> VoronoiFace a -> VoronoiFace a
updateFace p f = clipFace (perpendicularBisector (Line (center f) p)) f

clipFace :: Line -> VoronoiFace a -> VoronoiFace a
clipFace line f =
    case filter (pointInPolygon (center f)) (cutPolygon line (face f)) of
        [p] -> f { face = p }
        [] -> bugError "Could not identify the remaining Voronoi face. Perhaps the point was outside the face to start with?"
        _ -> bugError "`cutPolygon` resulted in overlapping polygons."
