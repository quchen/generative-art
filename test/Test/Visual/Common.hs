{-# LANGUAGE RecordWildCards #-}

module Test.Visual.Common (arrowSketch, renderAllFormats, hsva) where



import Graphics.Rendering.Cairo

import Draw
import Geometry



arrowSketch :: Line -> Render ()
arrowSketch line = do
    lineSketch line
    let Line _start end = line
        Angle rawAngle = angleOfLine line
    lineSketch (angledLine end (Angle (rawAngle + pi - 0.5)) (Distance 10))
    lineSketch (angledLine end (Angle (rawAngle + pi + 0.5)) (Distance 10))

renderPng :: Int -> Int -> FilePath -> Render () -> IO ()
renderPng picWidth picHeight filename drawing = do
    surface <- createImageSurface FormatARGB32 picWidth picHeight
    renderWith surface (do
        background picWidth picHeight
        drawing
        )
    surfaceWriteToPNG surface filename

background :: Int -> Int -> Render ()
background picWidth picHeight = do
    rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
    hsva 0 0 0 1
    setLineWidth 1
    stroke

renderSvg :: Int -> Int -> FilePath -> Render () -> IO ()
renderSvg picWidth picHeight filename drawing
  = withSVGSurface filename w h (\surface -> renderWith surface drawing)
  where
    w = fromIntegral picWidth
    h = fromIntegral picHeight

renderAllFormats :: Int -> Int -> FilePath -> Render () -> IO ()
renderAllFormats w h filename drawing = do
    renderPng w h (filename ++ ".png") drawing
    renderSvg w h (filename ++ ".svg") drawing
