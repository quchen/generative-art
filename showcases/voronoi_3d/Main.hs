{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Maybe         (fromMaybe)
import Data.List (sortOn)
import Data.Ord (comparing)
import Data.Vector        (fromList)
import Math.Noise         (Perlin (..), getValue, perlin)
import Prelude            hiding ((**))
import System.Random.MWC  (initialize)
import qualified Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Voronoi

picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

scaleFactor :: Double
scaleFactor = 1

main :: IO ()
main = do
    let file = "out/voronoi_3d.png"
        count = 500
        scaledWidth = round (scaleFactor * picWidth)
        scaledHeight = round (scaleFactor * picHeight)

    gen <- initialize (fromList [12, 984, 498, 498, 626, 15, 165])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = 1440 * sqrt (0.75 / count)
        samplingProps = PoissonDisc { width = 1440, height = 1440, radius = adaptiveRadius, k = 4, ..}

    points <- poissonDisc samplingProps
    print (length points)
    let voronoi = toVoronoi (lloydRelaxation $ bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 1440 1440)) points)
        voronoiWithHeight = mapWithSeed (const . randomHeight) voronoi
        origin = Vec2 (picWidth/2) (picHeight/2)
        voronoiCells = sortOn ((\(Polygon ps) -> minimum (yCoordinate <$> ps)) . fst) $
            ( \c ->
                ( transform
                    (  translate (Vec2 0 (picHeight/5))
                    <> scaleAround' origin 1 0.35
                    <> rotateAround origin (deg 45)
                    <> translate (Vec2 560 0)
                    )
                    (region c)
                , props c) )
            <$> cells voronoiWithHeight

    withSurfaceAuto file scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
        cairoScope (setColor black >> Cairo.paint)
        for_ voronoiCells $ uncurry drawCell

randomHeight :: Vec2 -> Double
randomHeight p = 300 + 400 * noise2d p + 200 * exp(- 0.000005 * normSquare (p -. origin))
  where
    noise = perlin { perlinOctaves = 4, perlinFrequency = 0.001, perlinSeed = 1980166}
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)
    origin = Vec2 720 720

drawCell :: Polygon -> Double -> Cairo.Render ()
drawCell (Polygon []) _ = pure ()
drawCell poly@(Polygon ps) height = cairoScope $ do
    let lineColor = hsva 180 1 0.5 1
        sideColor = blend 0.1 (dissolve 0.8 lineColor) (black `withOpacity` 0.3)
        topColor = blend 0.7 (dissolve 0.8 lineColor) (black `withOpacity` 0.3)

    Cairo.setLineJoin Cairo.LineJoinBevel

    for_ (zip normalizedPoints (drop 1 (cycle normalizedPoints))) $ \(p, q) -> cairoScope $ do
        moveToVec p
        lineToVec q
        lineToVec (transform (translate (Vec2 0 (-height))) q)
        lineToVec (transform (translate (Vec2 0 (-height))) p)
        setColor (dissolve 0.8 sideColor)
        Cairo.fillPreserve
        setColor lineColor
        Cairo.setLineWidth 2
        Cairo.stroke

    cairoScope $ do
        Cairo.translate 0 (-height)
        polygonSketch poly
        setColor topColor
        Cairo.fillPreserve
        setColor lineColor
        Cairo.setLineWidth 2
        Cairo.stroke

  where
    leftmost = minimumBy (comparing xCoordinate) ps
    normalizedPoints = rotateUntil (== leftmost) ps

rotateUntil :: (a -> Bool) -> [a] -> [a]
rotateUntil p xs = zipWith
    (flip const)
    xs
    (dropWhile (not . p) (cycle xs))

xCoordinate, yCoordinate :: Vec2 -> Double
xCoordinate (Vec2 x _) = x
yCoordinate (Vec2 _ y) = y
