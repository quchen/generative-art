{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Applicative             (liftA2)
import           Control.Monad                   (replicateM)
import           Data.Char                       (ord)
import           Data.Foldable                   (for_)
import           Data.List                       (find)
import           Data.Maybe                      (mapMaybe)
import           Data.Vector                     (fromList)
import           Prelude                         hiding ((**))
import           System.Environment              (getArgs)
import           System.Random.MWC               (GenIO, initialize)
import           System.Random.MWC.Distributions (normal)

import           Draw
import           Geometry
import           Geometry.Shapes                 (haskellLogo)
import           Graphics.Rendering.Cairo        hiding (transform)
import           Voronoi

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
        haskellLogoCentered = transform (translateT (Vec2 (w/2 - 480) (h/2 - 340)) <> scaleT 680 680) haskellLogo
    gen <- initialize (fromList (map (fromIntegral . ord) (show count)))
    points <- gaussianDistributedPoints gen (w, 380) (h, 380) count
    let picturePoints = mapMaybe (\point -> fmap (\(color, _) -> (point, color)) $ find (pointInPolygon point . snd) (zip haskellLogoColors haskellLogoCentered)) points
        backgroundPoints = fmap (\point -> (point, darkGrey)) $ filter (\point -> not $ any (pointInPolygon point) haskellLogoCentered) points
        allPoints = picturePoints ++ backgroundPoints
        allFaces = faces (mkVoronoi w h allPoints)
    for_ files $ \file -> withSurfaceAuto file w h $ \surface -> renderWith surface $ for_ allFaces drawFace

drawFace :: VoronoiFace RGB -> Render ()
drawFace VF{..} = drawPoly face props

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

gaussianDistributedPoints :: GenIO -> (Int, Double) -> (Int, Double) -> Int -> IO [Vec2]
gaussianDistributedPoints gen (width, sigmaX) (height, sigmaY) count = replicateM count randomPoint
  where
    randomPoint = liftA2 Vec2 (randomCoordinate width sigmaX) (randomCoordinate height sigmaY)
    randomCoordinate mx sigma = do
        coord <- normal (fromIntegral mx/2) sigma gen :: IO Double
        if coord < 0 || coord > fromIntegral mx
            then randomCoordinate mx sigma
            else pure coord


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
