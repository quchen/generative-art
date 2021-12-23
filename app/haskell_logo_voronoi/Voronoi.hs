{-# LANGUAGE RecordWildCards #-}
module Voronoi where

import           Data.List (foldl')

import           Geometry

data VoronoiFace = VF { center :: Vec2, face :: Polygon }
    deriving (Eq, Show)

data Voronoi = Voronoi { bounds :: Polygon, faces :: [VoronoiFace] }
    deriving (Eq, Show)

voronoi :: [Vec2] -> Double -> Double -> Voronoi
voronoi points w h = foldl' addPoint (emptyVoronoi w h) points

emptyVoronoi :: Double -> Double -> Voronoi
emptyVoronoi w h = Voronoi initialPolygon []
  where
    initialPolygon = Polygon [ Vec2 0 0, Vec2 w 0, Vec2 w h, Vec2 0 h ]

addPoint :: Voronoi -> Vec2 -> Voronoi
addPoint Voronoi{..} p = Voronoi bounds (newFace : faces')
  where
    newFace :: VoronoiFace
    newFace = foldl' (\nf f -> updateFace nf (center f)) (VF p bounds) faces

    faces' :: [VoronoiFace]
    faces' = fmap (\f -> updateFace f (center newFace)) faces

updateFace :: VoronoiFace -> Vec2 -> VoronoiFace
updateFace f p = clipFace f (Line q d)
  where
    q = let Vec2 px py = p
            Vec2 fx fy = center f
        in  Vec2 ((px + fx) / 2) ((py + fy) / 2)
    d = let Vec2 qx qy = q
            Vec2 dx dy = p -. center f
        in  Vec2 (qx - dy) (qy + dx)

-- | Keep everything that's *left* of the line
clipFace :: VoronoiFace -> Line -> VoronoiFace
clipFace f line =
    -- There is exactly one polygon containing the original center
    let [p] = filter (pointInPolygon (center f)) (cutPolygon line (face f))
    in  f { face = p }
