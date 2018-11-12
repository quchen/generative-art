{-# LANGUAGE RecordWildCards #-}

module Main where



import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry



picWidth, picHeight :: Int
picWidth = 600
picHeight = 400

main :: IO ()
main = do
    surface <- createImageSurface FormatARGB32 picWidth picHeight
    putStrLn "Rendering"
    renderWith surface sketch
    putStrLn "Writing output file"
    surfaceWriteToPNG surface "out/latest.png"

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v a = setSourceRGBA channelRed channelGreen channelBlue a
    where RGB{..} = hsv h s v

sketch :: Render ()
sketch = do
    do -- background
        rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
        hsva 0 0.7 0.5 0.1
        fill
    logoStroke


    let start = Vec2 390 300
        billardTable = Polygon [Vec2 200 200, Vec2 500 200, Vec2 500 380, Vec2 200 380]
        points = take 10 $ error "FIXME in Main.hs" $ billardProcess billardTable (angledLine start (deg 45) (Distance 10))
    hsva 130 0.7 0.5 1
    setLineWidth 1
    polygonSketch (Polygon points)
    stroke
    polygonSketch billardTable
    stroke
    circleSketch start (Distance 5)
    stroke

logoStroke :: Render ()
logoStroke = for_ movedLogo (\polygon -> do
    hsva 230 0.7 0.5 1
    polygonSketch polygon
    setLineWidth 2
    stroke)

movedLogo :: [Polygon]
movedLogo = move (Vec2 30 30) haskellLogo

haskellLogo :: [Polygon]
haskellLogo = [left, lambda, upper, lower]
  where
    left   = Polygon [Vec2 0 340.15625, Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625, Vec2 0 340.15625]
    lambda = Polygon [Vec2 113.386719 340.15625, Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625, Vec2 113.386719 340.15625]
    upper  = Polygon [Vec2 387.402344 240.945312, Vec2 349.609375 184.253906, Vec2 481.890625 184.25, Vec2 481.890625 240.945312, Vec2 387.402344 240.945312]
    lower  = Polygon [Vec2 330.710938 155.90625, Vec2 292.914062 99.214844, Vec2 481.890625 99.210938, Vec2 481.890625 155.90625, Vec2 330.710938 155.90625]
