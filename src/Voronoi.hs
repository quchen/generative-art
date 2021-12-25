{-# LANGUAGE RecordWildCards #-}
module Voronoi where

import           Data.List (foldl')

import           Geometry

data VoronoiFace a = VF { center :: Vec2, face :: Polygon, props :: a }
    deriving (Eq, Show)

data Voronoi a = Voronoi { bounds :: Polygon, faces :: [VoronoiFace a] }
    deriving (Eq, Show)

type Voronoi' = Voronoi ()

mkVoronoi :: [(Vec2, a)] -> Double -> Double -> Voronoi a
mkVoronoi taggedPoints w h = foldl' addPoint (emptyVoronoi w h) taggedPoints

mkVoronoi' :: [Vec2] -> Double -> Double -> Voronoi'
mkVoronoi' points w h = foldl' addPoint' (emptyVoronoi w h) points

emptyVoronoi :: Double -> Double -> Voronoi a
emptyVoronoi w h = Voronoi initialPolygon []
  where
    initialPolygon = Polygon [ Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h ]

addPoint :: Voronoi a -> (Vec2, a) -> Voronoi a
addPoint Voronoi{..} (p, a) = Voronoi bounds (newFace : faces')
  where
    newFace = foldl' (\nf f -> updateFace (center f) nf) (VF p bounds a) faces
    faces' = fmap (updateFace (center newFace)) faces

addPoint' :: Voronoi' -> Vec2 -> Voronoi'
addPoint' voronoi point = addPoint voronoi (point, ())

updateFace :: Vec2 -> VoronoiFace a -> VoronoiFace a
updateFace p f = clipFace (perpendicularBisector (Line (center f) p)) f

-- | Keep everything that's *left* of the line
clipFace :: Line -> VoronoiFace a -> VoronoiFace a
clipFace line f =
    -- There is exactly one polygon containing the original center
    let [p] = filter (pointInPolygon (center f)) (cutPolygon line (face f))
    in  f { face = p }
