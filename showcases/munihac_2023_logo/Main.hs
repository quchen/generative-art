module Main (main) where



import Control.Monad (guard)
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo     as C
import Prelude hiding ((**))
import System.Random.MWC

import Draw
import Geometry as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Voronoi
import Geometry.Shapes (haskellLogo)



-- ghcid --command='stack ghci generative-art:lib munihac2023logo --main-is munihac2023logo:exe:munihac2023logo' --test=main --warnings --no-title
main :: IO ()
main = mainHaskellLogoSmall

mainHaskellLogo :: IO ()
mainHaskellLogo = do
    let picWidth, picHeight :: Num a => a
        picWidth = 1000
        picHeight = 720
        lineWidth = 6
        count = 250
    gen <- initialize (V.fromList [11, 5])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = boundingBox [zero, Vec2 picWidth picHeight]
            , _poissonRadius = adaptiveRadius
            , _poissonK      = 4
            }
    points <- poissonDisc gen samplingProps
    print (length points)
    let voronoi = toVoronoi (lloydRelaxation 2 $ bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) points)

    let polygonsAndColors = do
            voronoiRegion <- _voronoiRegion <$> _voronoiCells voronoi
            (glyph, color) <- haskellLogoWithColorsShortened (picWidth, picHeight)
            (polygon, _) <- intersectionPP voronoiRegion (growPolygon (lineWidth/1.2) glyph)
            guard $ polygonArea polygon > 500
            pure (chaikin 0.25 $ chaikin 0.15 $ growPolygon (-lineWidth/1.2) polygon, color)

    let drawing = do
            cairoScope (setColor white >> C.paint)
            for_ polygonsAndColors $ \(polygon, color) ->
                drawPoly polygon color lineWidth
            for_ (haskellLogoWithColors (picWidth, picHeight)) $ \(poly, color) ->
                drawPoly poly color lineWidth
    render "munihac-2023-logo.png" picWidth picHeight drawing
    render "munihac-2023-logo.svg" picWidth picHeight drawing

mainHaskellLogoSmall :: IO ()
mainHaskellLogoSmall = do
    let picWidth, picHeight :: Num a => a
        picWidth = 30
        picHeight = 21
        lineWidth = 1
        count = 5
    gen <- initialize (V.fromList [2])
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = boundingBox [zero, Vec2 picWidth picHeight]
            , _poissonRadius = adaptiveRadius
            , _poissonK      = 4
            }
    points <- poissonDisc gen samplingProps
    print (length points)
    let voronoi = toVoronoi (lloydRelaxation 2 $ bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) points)

    let polygonsAndColors = do
            voronoiRegion <- _voronoiRegion <$> _voronoiCells voronoi
            (glyph, color) <- haskellLogoWithColors (picWidth, picHeight)
            (polygon, _) <- intersectionPP voronoiRegion (growPolygon (lineWidth/1.2) glyph)
            guard $ polygonArea polygon > 0
            pure (chaikin 0.25 $ chaikin 0.15 $ growPolygon (-lineWidth/1.3) polygon, color)

    let drawing = do
            cairoScope (setColor white >> C.paint)
            for_ polygonsAndColors $ \(polygon, color) ->
                drawPoly polygon color lineWidth
            --for_ (haskellLogoWithColors (picWidth, picHeight)) $ \(poly, color) ->
            --    drawPoly poly color lineWidth
    render "munihac-2023-logo-oneline.png" picWidth picHeight drawing
    render "munihac-2023-logo-oneline.svg" picWidth picHeight drawing

haskellLogoWithColors :: (Double, Double) -> [(Polygon, Color Double)]
haskellLogoWithColors (picWidth, picHeight) = zip haskellLogoCentered haskellLogoColors
  where
    haskellLogoCentered = G.transform (G.translate (Vec2 1 (picHeight/2 - 10)) <> G.scale 20) haskellLogo
    haskellLogoColors = [haskell 0, haskell 0.5, haskell 1, haskell 1]

haskellLogoWithColorsShortened :: (Double, Double) -> [(Polygon, Color Double)]
haskellLogoWithColorsShortened (picWidth, picHeight) = zip haskellLogoCentered haskellLogoColors
  where
    haskellLogoCentered = G.transform (G.translate (Vec2 (picWidth/2 - 480) (picHeight/2 - 340)) <> G.scale 680) $ rescaleNormalizePolygons haskellLogoRaw
    haskellLogoColors = [haskell 0, haskell 0.5, haskell 1, haskell 1]
    haskellLogoRaw = [left, lambda, upper, lower]
      where
        left   = Polygon [Vec2 0 340.15625, Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625]
        lambda = Polygon [Vec2 113.386719 340.15625, Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625]
        upper  = Polygon [Vec2 330.710938 155.90625, Vec2 292.914062 99.214844, Vec2 345.710938 99.210938, Vec2 345.710938 155.90625]
        lower  = Polygon [Vec2 387.402344 240.945312, Vec2 349.609375 184.253906, Vec2 392.402344 184.25, Vec2 392.402344 240.945312]
    rescaleNormalizePolygons polygons =
        let BoundingBox (Vec2 minX minY) (Vec2 _maxX maxY) = boundingBox polygons
            scaleFactor = 1 / (maxY - minY)
            transformation = scale scaleFactor <> translate (Vec2 (- minX) (- minY))
        in transform transformation polygons

drawPoly :: Polygon -> Color Double -> Double -> C.Render ()
drawPoly (Polygon []) _ _ = pure ()
drawPoly poly color lineWidth = do
    sketch poly
    setColor color
    C.setLineWidth lineWidth
    C.stroke

chaikin :: Double -> Polygon -> Polygon
chaikin _ (Polygon []) = Polygon []
chaikin lambda (Polygon ps@(p:_)) = Polygon $ concat
    [ [c, b]
    | (a, d) <- zip ps (tail ps ++ [p])
    , let b = lambda *. a +. (1-lambda) *. d
    , let c = (1-lambda) *. a +. lambda *. d
    ]
