{-# LANGUAGE RecordWildCards #-}
module Main (main, main0, main1, main2, main3) where



import Data.List ( sortOn )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as C
import Math.Noise (Perlin (..), getValue, perlin)
import System.Random.MWC ( initialize, uniformRM )

import Draw
import Geometry as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Voronoi
import Control.Monad (replicateM)
import Control.Applicative (Applicative(liftA2))



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

file :: FilePath 
file = "out/voronoi_3d.png"

main :: IO ()
main = main3

main0 :: IO ()
main0 = do
    points <- uniformDistribution 500
    withSurfaceAuto file 1440 1440 $ \surface -> renderWith surface $ do
        cairoScope (setColor white >> paint)
        for_ points $ \point -> cairoScope $ do
            circleSketch point 5
            setColor black
            fill

uniformDistribution :: Int -> IO [Vec2]
uniformDistribution count = do
    gen <- initialize (V.fromList [12, 984, 498, 498, 626, 15, 165])
    replicateM count $ liftA2 Vec2
        (uniformRM (0, 1440) gen)
        (uniformRM (0, 1440) gen)
 
--------------------------------------------------------------------------------

main1 :: IO ()
main1 = do
    points <- uniformDistribution 500

    let voronoi = toVoronoi (bowyerWatson extents points)
        voronoiCells = cells voronoi

    withSurfaceAuto file 1440 1440 $ \surface -> renderWith surface $ do
        cairoScope (setColor white >> paint)
        for_ voronoiCells $ \cell -> cairoScope $ do
            circleSketch (seed cell) 5
            setColor black
            fill
        for_ voronoiCells drawCell
  where
    extents = BoundingBox (Vec2 0 0) (Vec2 1440 1440)

poissonDiscDistribution :: Int -> IO [Vec2]
poissonDiscDistribution count = do
    gen <- initialize (V.fromList [12, 984, 498, 498, 626, 15, 165])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = 1440 * sqrt (0.75 / fromIntegral count)
        samplingProps = PoissonDisc { width = 1440, height = 1440, radius = adaptiveRadius, k = 4, ..}

    points <- poissonDisc samplingProps
    print (length points)
    pure points

drawCell :: VoronoiCell a -> Render ()
drawCell Cell{..} = cairoScope $ do
    C.setLineJoin C.LineJoinBevel
    polygonSketch poly
    setColor black
    setLineWidth 2
    stroke
  where
    poly = region

--------------------------------------------------------------------------------

main2 :: IO ()
main2 = do
    points <- poissonDiscDistribution 500

    let voronoi = toVoronoi (lloydRelaxation $ lloydRelaxation $ lloydRelaxation $ lloydRelaxation $ bowyerWatson extents points)
        voronoiWithProps = mapWithSeed (\p () -> randomColor p) voronoi
        voronoiCells = cells voronoiWithProps

    withSurfaceAuto file 1440 1440 $ \surface -> renderWith surface $ do
        cairoScope (setColor (magma 0.05) >> paint)
        for_ voronoiCells drawCellColor
  where
    extents = BoundingBox (Vec2 0 0) (Vec2 1440 1440)

randomColor :: Vec2 -> Color Double
randomColor = \p -> inferno (0.6 + 0.35 * noise2d p)
  where
    noise = perlin { perlinOctaves = 5, perlinFrequency = 0.001, perlinPersistence = 0.65, perlinSeed = 1980166 }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)

drawCellColor :: VoronoiCell (Color Double) -> Render ()
drawCellColor Cell{..} = cairoScope $ do
    let lineColor = blend 0.95 color white
        cellColor = blend 0.60 color black

    C.setLineJoin C.LineJoinBevel

    polygonSketch poly
    setColor cellColor
    fillPreserve
    setColor lineColor
    setLineWidth 2
    stroke

  where
    poly = region
    color = props

--------------------------------------------------------------------------------

main3 :: IO ()
main3 = do
    points <- poissonDiscDistribution 500

    let voronoi = toVoronoi (lloydRelaxation $ lloydRelaxation $ lloydRelaxation $ lloydRelaxation $ bowyerWatson extents points)
        voronoiWithProps = mapWithSeed (\p () -> (randomColor p, randomHeight p)) voronoi
        voronoiCells = sortOn (backToFront . region) $ cells voronoiWithProps

    withSurfaceAuto file 2560 1440 $ \surface -> renderWith surface $ do
        cairoScope (setColor (magma 0.05) >> paint)
        for_ voronoiCells drawColumn
  where
    extents = BoundingBox (Vec2 0 0) (Vec2 1440 1440)
    backToFront (Polygon ps) = minimum (yCoordinate <$> ps)

randomHeight :: Vec2 -> Double
randomHeight = \p -> 300 + 400 * noise2d p + 200 * exp(- 0.000005 * normSquare (p -. origin))
  where
    noise = perlin { perlinOctaves = 4, perlinFrequency = 0.001, perlinSeed = 1980166 }
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)
    origin = Vec2 720 720

drawColumn :: VoronoiCell (Color Double, Double) -> Render ()
drawColumn Cell{..} = cairoScope $ do
    let lineColor = blend 0.95 color white
        sideColor = blend 0.1 (color `withOpacity` 0.8) (black `withOpacity` 0.3)
        cellColor = blend 0.7 (color `withOpacity` 0.8) (black `withOpacity` 0.3)

    C.setLineJoin C.LineJoinBevel

    for_ (zip normalizedPoints (drop 1 (cycle normalizedPoints))) $ \(p, q) -> cairoScope $ do
        moveToVec p
        lineToVec q
        lineToVec (G.transform (G.translate (Vec2 0 (-height))) q)
        lineToVec (G.transform (G.translate (Vec2 0 (-height))) p)
        setColor (dissolve 0.8 sideColor)
        fillPreserve
        setColor lineColor
        setLineWidth 2
        stroke

    cairoScope $ do
        C.translate 0 (-height)
        polygonSketch poly
        setColor cellColor
        fillPreserve
        setColor lineColor
        setLineWidth 2
        stroke
  where
    poly@(Polygon ps) = G.transform (isometricPerspective <> G.scaleAround seed 0.9) region
    (color, height) = props
    leftmost = minimumBy (comparing xCoordinate) ps
    normalizedPoints = rotateUntil (== leftmost) ps

isometricPerspective :: Transformation
isometricPerspective =
       G.translate (Vec2 0 (picHeight/5))
    <> G.scaleAround' origin 1 0.35
    <> G.rotateAround origin (deg 45)
    <> G.translate (Vec2 560 0)
  where
    origin = Vec2 (picWidth/2) (picHeight/2)

rotateUntil :: (a -> Bool) -> [a] -> [a]
rotateUntil p xs = zipWith
    (flip const)
    xs
    (dropWhile (not . p) (cycle xs))

xCoordinate, yCoordinate :: Vec2 -> Double
xCoordinate (Vec2 x _) = x
yCoordinate (Vec2 _ y) = y
