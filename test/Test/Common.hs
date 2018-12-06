module Test.Common
    ( angleSketch
    , renderAllFormats
    , hsva
    , haskellLogo
    , wonkyHaskellLogo
    , spiralPolygon
    ) where



import Graphics.Rendering.Cairo hiding (x,y, rotate, width)
import System.Random

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
renderPng picWidth picHeight filename drawing = do
    surface <- createImageSurface FormatARGB32 picWidth picHeight
    renderWith surface $ do
        restoreStateAfter $ do
            rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
            hsva 0 0 0 0
            setLineWidth 0
            fill
        drawing
    surfaceWriteToPNG surface filename

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

haskellLogo :: [Polygon]
haskellLogo = [left, lambda, upper, lower]
  where
    left   = Polygon [Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625, Vec2 0 340.15625]
    lambda = Polygon [Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625, Vec2 113.386719 340.15625]
    upper  = Polygon [Vec2 292.914062 99.214844, Vec2 481.890625 99.210938, Vec2 481.890625 155.90625, Vec2 330.710938 155.90625]
    lower  = Polygon [Vec2 349.609375 184.253906, Vec2 481.890625 184.25, Vec2 481.890625 240.945312, Vec2 387.402344 240.945312]

wonkyHaskellLogo :: [Polygon]
wonkyHaskellLogo = map wigglePoly haskellLogo
  where
    wigglePoly :: Polygon -> Polygon
    wigglePoly (Polygon corners) = Polygon (map wiggle corners)
    wiggle :: Vec2 -> Vec2
    wiggle v@(Vec2 x y)
      = let seed = let (x1,x2) = decodeFloat x
                       (y1,y2) = decodeFloat y
                   in fromIntegral x1 + x2 + fromIntegral y1 + y2 + 1
            (angle, _gen') = randomR (0, 360) (mkStdGen seed)
        in moveRad (Angle angle) (Distance 10) v

spiralPolygon :: Int -> Double -> Polygon
spiralPolygon n width = Polygon (scanl (+.) (Vec2 0 0) relativeSpiral)
  where
    instructions = concat [ zip [1..n] (repeat turnLeft)
                          , [(1, turnLeft)]
                          , [(n-1, turnRight)]
                          , zip [n-3, n-4 .. 1] (repeat turnRight)
                          ]
    relativeSpiral = go instructions (Vec2 1 0)
      where
        go [] _dir = []
        go ((len, rotate) : rest) dir = width*fromIntegral len *. dir : go rest (rotate dir)
    turnLeft  (Vec2 x y) = Vec2   y  (-x)
    turnRight (Vec2 x y) = Vec2 (-y)   x
