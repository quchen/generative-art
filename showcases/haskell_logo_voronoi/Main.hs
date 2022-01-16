{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Char          (ord)
import Data.Foldable      (for_, foldl')
import Data.List          (find)
import Data.Maybe         (fromMaybe)
import Data.Vector        (fromList)
import Math.Noise         (Perlin (..), getValue, perlin)
import Prelude            hiding ((**))
import System.Environment (getArgs)
import System.Random.MWC  (initialize)

import Delaunay
import Draw
import Geometry
import Geometry.Shapes          (haskellLogo)
import Graphics.Rendering.Cairo hiding (transform)
import Sampling
import qualified Util.RTree as RT
import Voronoi

picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 720

main :: IO ()
main = mainHaskellLogo

mainHaskellLogo :: IO ()
mainHaskellLogo = do
    let defaultFile = "out/haskell_logo_voronoi.svg"
    (count, file) <- getArgs >>= \case
        [] -> pure (1000, defaultFile)
        [count] -> pure (read count, defaultFile)
        [count, file] -> pure (read count, file)
        _ -> error "Usage: haskell-logo-voronoi [COUNT [FILE]]"
    gen <- initialize (fromList (map (fromIntegral . ord) (show count)))
    let -- constructed so that we have roughly `count` points
        adaptiveRadius = sqrt (0.75 * picWidth * picHeight / count)
        samplingProps = PoissonDisc { width = picWidth, height = picHeight, radius = adaptiveRadius, k = 4, ..}
    points <- poissonDisc samplingProps
    ditheringPoints <- RT.fromList <$> poissonDisc samplingProps { radius = adaptiveRadius / 4 }
    print (length points)
    let voronoi = toVoronoi (bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) points)
        voronoiColorized = mapWithRegion (colorizePolygon ditheringPoints) voronoi

    withSurfaceAuto file picWidth picHeight $ \surface -> renderWith surface $ for_ (cells voronoiColorized) drawCell

haskellLogoWithColors :: [(Polygon, RGB)]
haskellLogoWithColors = zip haskellLogoCentered haskellLogoColors
  where
    haskellLogoCentered = transform (Geometry.translate (Vec2 (picWidth/2 - 480) (picHeight/2 - 340)) <> Geometry.scale 680) haskellLogo
    haskellLogoColors = fmap parseHex [ "453a62", "5e5086", "8f4e8b", "8f4e8b" ]


findPointsInPolygon :: RT.RTree Vec2 -> Polygon -> [Vec2]
findPointsInPolygon points poly = filter (`pointInPolygon` poly) (RT.lookupRange (boundingBox poly) points)

colorizePolygon :: RT.RTree Vec2 -> Polygon -> () -> RGB
colorizePolygon ditheringPoints voronoiRegion _ = average $ colorizePoint <$> ditheringPointsInRegion
  where
    ditheringPointsInRegion = findPointsInPolygon ditheringPoints voronoiRegion
    colorizePoint p
        | Just (_, color) <- find (pointInPolygon p . fst) haskellLogoWithColors
            = color +. 0.1 *. grey (noise2d p)
        | otherwise
            = darkGrey +. 0.1 *. grey (noise2d p)
    noise = perlin { perlinFrequency = 40/picWidth, perlinSeed = 12345}
    noise2d (Vec2 x y) = fromMaybe 0 $ getValue noise (x, y, 0)

drawCell :: VoronoiCell RGB -> Render ()
drawCell Cell{..} = drawPoly region props

drawPoly :: Polygon -> RGB -> Render ()
drawPoly (Polygon []) _ = pure ()
drawPoly poly color = do
    let fillColor = color
        lineColor = lighten 0.2 color
    polygonSketch poly
    setColor fillColor
    fillPreserve
    setColor lineColor
    setLineWidth 1
    stroke

data RGB = RGB { r :: Double, g :: Double, b :: Double }

instance VectorSpace RGB where
    RGB r1 g1 b1 +. RGB r2 g2 b2 = RGB (r1+r2) (g1+g2) (b1+b2)
    RGB r1 g1 b1 -. RGB r2 g2 b2 = RGB (r1-r2) (g1-g2) (b1-b2)
    a *. RGB{..} = RGB (a*r) (a*g) (a*b)
    zero = RGB 0 0 0

setColor :: RGB -> Render ()
setColor RGB{..} = setSourceRGB r g b

parseHex :: String -> RGB
parseHex [r1, r2, g1, g2, b1, b2] = RGB
    { r = read ("0x" ++ [r1, r2]) / 255
    , g = read ("0x" ++ [g1, g2]) / 255
    , b = read ("0x" ++ [b1, b2]) / 255 }
parseHex _ = undefined

grey :: Double -> RGB
grey shade = RGB shade shade shade

darkGrey :: RGB
darkGrey = RGB 0.1 0.1 0.1

lighten :: Double -> RGB -> RGB
lighten d color = color +. RGB d d d

average :: [RGB] -> RGB
average colors = foldl' (+.) zero colors /. count
  where
    count = fromIntegral $ length colors
