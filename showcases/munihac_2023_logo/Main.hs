{-# LANGUAGE RecordWildCards #-}
module Main (main) where



import           Data.Char
import           Data.Maybe
import qualified Data.Vector         as V
import           Prelude             hiding ((**))
import           System.Random.MWC

import           Draw
import           Geometry                     as G
import           Geometry.Algorithms.Delaunay
import           Geometry.Algorithms.Sampling
import           Geometry.Algorithms.Voronoi
import           Geometry.Shapes              (haskellLogo)
import           Graphics.Rendering.Cairo     as C



-- ghcid --command='stack ghci generative-art:lib munihac2023logo --main-is munihac2023logo:exe:munihac2023logo' --test=main --warnings --no-title
main :: IO ()
main = mainHaskellLogo

mainHaskellLogo :: IO ()
mainHaskellLogo = do
    let picWidth, picHeight :: Num a => a
        picWidth = 1000
        picHeight = 720
        count = 400
    gen <- initialize (V.fromList (map (fromIntegral . ord) (show count)))
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / fromIntegral count)
        samplingProps = PoissonDiscParams
            { _poissonShape = boundingBox [zero, Vec2 picWidth picHeight]
            , _poissonRadius = adaptiveRadius
            , _poissonK      = 4
            }
    points <- poissonDisc gen samplingProps
    print (length points)
    let voronoi = toVoronoi (bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) points)
    let voronoiColorized = mapWithMetadata (\seed _polygon ann -> colorizePolygon (picWidth, picHeight) seed ann) voronoi

    render "munihac-2023-logo.png" picWidth picHeight $ for_ (_voronoiCells voronoiColorized) drawCell

haskellLogoWithColors :: (Double, Double) -> [(Polygon, Color Double)]
haskellLogoWithColors (picWidth, picHeight)= zip haskellLogoCentered haskellLogoColors
  where
    haskellLogoCentered = G.transform (G.translate (Vec2 (picWidth/2 - 480) (picHeight/2 - 340)) <> G.scale 680) haskellLogo
    haskellLogoColors = [haskell 0, haskell 1, haskell 2, haskell 2]


colorizePolygon :: (Double, Double) -> Vec2 -> () -> Color Double
colorizePolygon (w, h) seed _ = case find (pointInPolygon seed . fst) (haskellLogoWithColors (w, h)) of
            Just (_, c) -> c
            Nothing     -> darkGrey

drawCell :: VoronoiCell (Color Double) -> Render ()
drawCell VoronoiCell{..} = drawPoly _voronoiRegion _voronoiProps

drawPoly :: Polygon -> Color Double -> Render ()
drawPoly (Polygon []) _ = pure ()
drawPoly poly color = do
    let fillColor = color
        lineColor = blend 0.1 white color
    sketch poly
    setColor fillColor
    fillPreserve
    setColor lineColor
    setLineWidth 1
    stroke

darkGrey :: Color Double
darkGrey = hsv 0 0 0.1
