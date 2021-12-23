{-# LANGUAGE RecordWildCards #-}
module Voronoi where

import           Data.List (foldl')

import           Geometry

data VoronoiFace a = VF { center :: Vec2, face :: Polygon, props :: a }
    deriving (Eq, Show)

data Voronoi a = Voronoi { bounds :: Polygon, faces :: [VoronoiFace a] }
    deriving (Eq, Show)

type Voronoi' = Voronoi ()

voronoi :: [(Vec2, a)] -> Double -> Double -> Voronoi a
voronoi taggedPoints w h = foldl' addPoint (emptyVoronoi w h) taggedPoints

voronoi' :: [Vec2] -> Double -> Double -> Voronoi'
voronoi' points w h = foldl' addPoint' (emptyVoronoi w h) points

emptyVoronoi :: Double -> Double -> Voronoi a
emptyVoronoi w h = Voronoi initialPolygon []
  where
    initialPolygon = Polygon [ Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h ]

addPoint :: Voronoi a -> (Vec2, a) -> Voronoi a
addPoint Voronoi{..} (p, a) = Voronoi bounds (newFace : faces')
  where
    newFace = foldl' (\nf f -> updateFace nf (center f)) (VF p bounds a) faces
    faces' = fmap (\f -> updateFace f (center newFace)) faces

addPoint' :: Voronoi' -> Vec2 -> Voronoi'
addPoint' voronoi point = addPoint voronoi (point, ())

updateFace :: VoronoiFace a -> Vec2 -> VoronoiFace a
updateFace f p = clipFace f (Line q d)
  where
    q = let Vec2 px py = p
            Vec2 fx fy = center f
        in  Vec2 ((px + fx) / 2) ((py + fy) / 2)
    d = let Vec2 qx qy = q
            Vec2 dx dy = p -. center f
        in  Vec2 (qx - dy) (qy + dx)

-- | Keep everything that's *left* of the line
clipFace :: VoronoiFace a -> Line -> VoronoiFace a
clipFace f line =
    -- There is exactly one polygon containing the original center
    let [p] = filter (pointInPolygon (center f)) (cutPolygon line (face f))
    in  f { face = p }
