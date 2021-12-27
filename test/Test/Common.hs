module Test.Common
    ( angleSketch
    , renderAllFormats
    , hsva
    ) where



import Graphics.Rendering.Cairo as Cairo hiding (rotate, width, x, y)

import Draw
import Geometry



angleSketch :: Vec2 -> Angle -> Angle -> Render ()
angleSketch point angle1 angle2@(Angle rawAngle2) = do
    let radius = Distance 10
    arcSketch point radius angle1 angle2
    let arcEnd = moveRad angle2 radius point
        arrowAngleTweak = -0.2
        tangentStart = moveRad (Angle (rawAngle2 - pi/2 + arrowAngleTweak)) radius arcEnd
        tangent = Line tangentStart arcEnd
    arrowSketch tangent def
        { arrowheadSize = Distance 6
        , arrowDrawBody = False }

renderPng :: Int -> Int -> FilePath -> Render () -> IO ()
renderPng picWidth picHeight filename drawing = withPNGSurface filename picWidth picHeight $ \surface ->
    renderWith surface $ do
        restoreStateAfter $ do
            rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
            hsva 0 0 0 0
            setLineWidth 0
            fill
        drawing

renderSvg :: Int -> Int -> FilePath -> Render () -> IO ()
renderSvg picWidth picHeight filename drawing
  = withSVGSurface filename w h (\surface -> renderWith surface drawing)
  where
    w = fromIntegral picWidth
    h = fromIntegral picHeight

renderAllFormats :: Int -> Int -> FilePath -> Render () -> IO ()
renderAllFormats w h filename drawing = do
    if False then renderPng w h (filename ++ ".png") drawing else pure ()
    renderSvg w h (filename ++ ".svg") drawing
