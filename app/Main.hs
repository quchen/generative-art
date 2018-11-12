{-# LANGUAGE RecordWildCards #-}

module Main where



import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable
import Graphics.Rendering.Cairo

import Draw
import Geometry



picWidth, picHeight :: Num a => a
picWidth = 502
picHeight = 360

main :: IO ()
main = png >> svg
  where
    png = do
        surface <- createImageSurface FormatARGB32 picWidth picHeight
        renderWith surface drawing
        surfaceWriteToPNG surface "out/haskell_logo_billard.png"
    svg = withSVGSurface "out/haskell_logo_billard.svg" picWidth picHeight (\surface -> renderWith surface drawing)

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v a = setSourceRGBA channelRed channelGreen channelBlue a
    where RGB{..} = hsv h s v

drawing :: Render ()
drawing = do
    let [left, lambda, upper, lower] = haskellLogo
        billardLeft   = take 256 (drop 1 (billardProcess left   (angledLine (Vec2 10 10) (deg 40) (Distance 10))))
        billardLambda = take 400 (drop 1 (billardProcess lambda (angledLine (Vec2 230 175) (deg 40) (Distance 10))))
        billardUpper  = take 120 (drop 1 (billardProcess upper  (angledLine (Vec2 400 120) (deg 20) (Distance 10))))
        billardLower  = take 120 (drop 1 (billardProcess lower  (angledLine (Vec2 450 220) (deg 40) (Distance 10))))
        drawBillard points = do
            let billardLines = zipWith Line points (tail points)
            setLineWidth 1
            for_ billardLines (\line -> do
                lineSketch line
                stroke )

    translate 10 10
    hsva 257 0.40 0.38 1
    drawBillard billardLeft
    polygonSketch left >> stroke
    hsva 256 0.40 0.50 1
    drawBillard billardLambda
    polygonSketch lambda >> stroke
    hsva 304 0.45 0.56 1
    drawBillard billardUpper
    drawBillard billardLower
    polygonSketch upper >> stroke
    polygonSketch lower >> stroke

haskellLogo :: [Polygon]
haskellLogo = [left, lambda, upper, lower]
  where
    left   = Polygon [Vec2 0 340.15625, Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625, Vec2 0 340.15625]
    lambda = Polygon [Vec2 113.386719 340.15625, Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625, Vec2 113.386719 340.15625]
    upper  = Polygon [Vec2 330.710938 155.90625, Vec2 292.914062 99.214844, Vec2 481.890625 99.210938, Vec2 481.890625 155.90625, Vec2 330.710938 155.90625]
    lower  = Polygon [Vec2 387.402344 240.945312, Vec2 349.609375 184.253906, Vec2 481.890625 184.25, Vec2 481.890625 240.945312, Vec2 387.402344 240.945312]
