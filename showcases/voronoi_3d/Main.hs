{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Maybe         (fromMaybe)
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
        count = 1000
        scaledWidth = round (scaleFactor * picWidth)
        scaledHeight = round (scaleFactor * picHeight)

    gen <- initialize (fromList [12, 984, 498, 498, 626, 15, 165])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = 1440 * sqrt (0.75 / count)
        samplingProps = PoissonDisc { width = 1440, height = 1440, radius = adaptiveRadius, k = 4, ..}

    points <- poissonDisc samplingProps
    print (length points)
    let voronoi = toVoronoi (bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 1440 1440)) points)
        voronoiWithHeight = mapWithSeed (const . randomHeight) voronoi
        origin = Vec2 (picWidth/2) (picHeight/2)
        voronoiCells =
            (\c ->
                ( transform
                    (  translate (Vec2 0 (picHeight/4))
                    <> scaleAround' origin 1 0.3
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
randomHeight p = noise2d p - 2 - 2 * exp(- 0.000005 * normSquare (p -. origin))
  where
    noise = perlin { perlinFrequency = 0.1, perlinSeed = 1980165}
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)
    origin = Vec2 720 720

drawCell :: Polygon -> Double -> Cairo.Render ()
drawCell (Polygon []) _ = pure ()
drawCell poly height = cairoScope $ do
    Cairo.translate 0 (100 * height)
    let fillColor = hsva 180 1 0.3 0.9
        lineColor = black
    polygonSketch poly
    setColor fillColor
    Cairo.fillPreserve
    setColor lineColor
    Cairo.setLineWidth 1
    Cairo.stroke
