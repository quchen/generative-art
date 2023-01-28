{-# LANGUAGE RecordWildCards #-}
module Main (main, main0, main1, main2, main3) where



import Data.Colour.Names
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
import Geometry.Algorithms.Sampling.Vogel
import Geometry.Algorithms.Voronoi
import Control.Monad (replicateM, when)
import Control.Applicative (Applicative(liftA2))



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

file :: FilePath
file = "out/voronoi_3d.png"

main :: IO ()
main = main1

main0 :: IO ()
main0 = do
    let points = vogel VogelSamplingParams
            { _vogelRadius = 720
            , _vogelCenter = Vec2 720 720
            , _vogelDensity = 0.001
            }
    render file 1440 1440 $ do
        cairoScope (setColor white >> paint)
        for_ points $ \point -> cairoScope $ do
            sketch (Circle point 5)
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
    let center = Vec2 (picWidth/2) (picHeight/2)
    let points = filter (`insideBoundingBox` extents) $ vogel VogelSamplingParams
            { _vogelRadius = 1440
            , _vogelCenter = center
            , _vogelDensity = 0.0004
            }
        cutoff = 1080

    let delaunay = lloydRelaxation 5 (bowyerWatson extents points)
        voronoi = toVoronoi delaunay
        backToFront (Polygon ps) = maximum (yCoordinate <$> ps)
        voronoiCells = sortOn (backToFront . _voronoiRegion) $ filter (\VoronoiCell{..} -> norm (_voronoiSeed -. center) < cutoff) $ _voronoiCells voronoi

    render file picWidth picHeight $ do
        cairoScope (setColor white >> paint)
        for_ (getPolygons delaunay) $ \poly@(Polygon ps) -> cairoScope $ do
            when (all (\p -> norm (p -. center) < cutoff) ps) $ do
                setColor (blend 0.5 white black)
                sketch (G.transform isometricPerspective poly)
                C.stroke
        for_ voronoiCells $ \cell@VoronoiCell{..} -> cairoScope $ do
            let cellGutter = 6 - norm (Vec2 720 720 -. _voronoiSeed) / (18*12)
            let l = 1 -- norm (center -. _voronoiSeed) / cutoff
            let h = 240 - 38*log (40 + norm (center -. _voronoiSeed))
            for_ [0 .. h] $ \y -> cairoScope $ do
                C.translate 0 (-y)
                setColor (black `withOpacity` l)
                drawCell cell { _voronoiRegion = growPolygon (-cellGutter) _voronoiRegion }
  where
    extents = BoundingBox (Vec2 0 (-picWidth)) (Vec2 picWidth (2*picWidth))

poissonDiscDistribution :: Int -> IO [Vec2]
poissonDiscDistribution count = do
    gen <- initialize (V.fromList [12, 984, 498, 498, 626, 15, 165])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = 1440 * sqrt (0.75 / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = boundingBox [Vec2 0 0, Vec2 1440 1440]
            , _poissonRadius = adaptiveRadius
            , _poissonK = 4
            }

    points <- poissonDisc gen samplingProps
    print (length points)
    pure points

drawCell :: VoronoiCell a -> Render ()
drawCell VoronoiCell{..} = cairoScope $ do
    C.setLineJoin C.LineJoinBevel
    sketch (Polygon ps)
    strokePreserve
    setColor white
    fill
  where
    Polygon ps = G.transform isometricPerspective $ chaikin 0.25 (chaikin 0.25 (chaikin 0.15 _voronoiRegion))

chaikin :: Double -> Polygon -> Polygon
chaikin _ (Polygon []) = Polygon []
chaikin lambda (Polygon ps@(p:_)) = Polygon $ concat
    [ [c, b]
    | (a, d) <- zip ps (tail ps ++ [p])
    , let b = lambda *. a +. (1-lambda) *. d
    , let c = (1-lambda) *. a +. lambda *. d
    ]


--------------------------------------------------------------------------------

main2 :: IO ()
main2 = do
    points <- poissonDiscDistribution 500

    let voronoi = toVoronoi (lloydRelaxation 4 $ bowyerWatson extents points)
        voronoiWithProps = mapWithMetadata (\p _ _ -> randomColor p) voronoi
        voronoiCells = _voronoiCells voronoiWithProps

    render file 1440 1440 $ do
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
drawCellColor VoronoiCell{..} = cairoScope $ do
    let lineColor = blend 0.95 color white
        cellColor = blend 0.60 color black

    C.setLineJoin C.LineJoinBevel

    sketch poly
    setColor cellColor
    fillPreserve
    setColor lineColor
    setLineWidth 2
    stroke

  where
    poly = _voronoiRegion
    color = _voronoiProps

--------------------------------------------------------------------------------

main3 :: IO ()
main3 = do
    points <- poissonDiscDistribution 500

    let voronoi = toVoronoi (lloydRelaxation 4 $ bowyerWatson extents points)
        voronoiWithProps = mapWithMetadata (\p _ _ -> (randomColor p, randomHeight p)) voronoi
        voronoiCells = sortOn (backToFront . _voronoiRegion) $ _voronoiCells voronoiWithProps

    render file 2560 1440 $ do
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
drawColumn VoronoiCell{..} = cairoScope $ do
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
        sketch poly
        setColor cellColor
        fillPreserve
        setColor lineColor
        setLineWidth 2
        stroke
  where
    poly@(Polygon ps) = G.transform (isometricPerspective <> G.scaleAround _voronoiSeed 0.9) _voronoiRegion
    (color, height) = _voronoiProps
    leftmost = minimumBy (comparing xCoordinate) ps
    normalizedPoints = rotateUntil (== leftmost) ps

isometricPerspective :: Transformation
isometricPerspective =
       G.translate (Vec2 0 (picHeight/5))
    <> G.scaleAround' origin 1 0.35
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
