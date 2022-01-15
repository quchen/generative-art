{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Char          (ord)
import Data.Foldable      (for_)
import Data.List          (find)
import Data.Maybe         (mapMaybe)
import Data.Vector        (fromList)
import Prelude            hiding ((**))
import System.Environment (getArgs)
import System.Random.MWC  (initialize)

import Draw
import Geometry
import Geometry.Shapes          (haskellLogo)
import Graphics.Rendering.Cairo hiding (transform)
import Sampling
import Voronoi
import Delaunay

data RGB = RGB { r :: Double, g :: Double, b :: Double }

main :: IO ()
main = mainHaskellLogo

mainHaskellLogo :: IO ()
mainHaskellLogo = do
    let defaultFiles = ("out/haskell_logo_voronoi" ++) <$> [".png", ".svg"]
    (count, files) <- getArgs >>= \case
        [] -> pure (2048, defaultFiles)
        [count] -> pure (read count, defaultFiles)
        [count, file] -> pure (read count, [file])
        _ -> error "Usage: haskell-logo-voronoi [COUNT [FILE]]"
    let w, h :: Num a => a
        w = 1200
        h = 1200
        haskellLogoCentered = transform (Geometry.translate (Vec2 (w/2 - 480) (h/2 - 340)) <> Geometry.scale 680) haskellLogo
    gen <- initialize (fromList (map (fromIntegral . ord) (show count)))
    points <- gaussianDistributedPoints gen (w, 380) (h, 380) count
    let --picturePoints = mapMaybe (\point -> fmap (\(color, _) -> (point, color)) $ find (pointInPolygon point . snd) (zip haskellLogoColors haskellLogoCentered)) points
        --backgroundPoints = fmap (\point -> (point, darkGrey)) $ filter (\point -> not $ any (pointInPolygon point) haskellLogoCentered) points
        --allPoints = picturePoints ++ backgroundPoints
        allPoints = points
        allCells = cells (const darkGrey <$> toVoronoi (bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 w h)) points))
    for_ files $ \file -> withSurfaceAuto file w h $ \surface -> renderWith surface $ for_ allCells drawCell

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

setColor :: RGB -> Render ()
setColor RGB{..} = setSourceRGB r g b

haskellLogoColors :: [RGB]
haskellLogoColors = fmap parseHex [ "453a62", "5e5086", "8f4e8b", "8f4e8b" ]

parseHex :: String -> RGB
parseHex [r1, r2, g1, g2, b1, b2] = RGB
    { r = read ("0x" ++ [r1, r2]) / 255
    , g = read ("0x" ++ [g1, g2]) / 255
    , b = read ("0x" ++ [b1, b2]) / 255 }
parseHex _ = undefined

darkGrey :: RGB
darkGrey = RGB 0.1 0.1 0.1

lighten :: Double -> RGB -> RGB
lighten d RGB{..} = RGB (r + d) (g + d) (b + d)
