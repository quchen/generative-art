module Main where



import Graphics.Rendering.Cairo
import System.Random
import Data.Foldable
import Control.Monad
import Data.Time.Clock.POSIX
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Semigroup

import Geometry



picWidth, picHeight :: Int
picWidth = 600
picHeight = 400

main :: IO ()
main = do
    seed <- fmap (round . (* 1000)) getPOSIXTime
    surface <- createImageSurface FormatARGB32 picWidth picHeight
    putStrLn "Rendering"
    renderSketch surface
    -- surfaceWriteToPNG surface ("out/" <> show seed <> ".png")
    surfaceWriteToPNG surface "out/latest.png"

renderSketch :: Surface -> IO ()
renderSketch surface = renderWith surface sketch

hsva h s v a = setSourceRGBA channelRed channelGreen channelBlue a
    where RGB{channelRed = channelRed, channelGreen = channelGreen, channelBlue = channelBlue} = hsv h s v

sketch :: Render ()
sketch = do
    background
    logoStroke
    randomPoints
    -- scanlines

logoStroke :: Render ()
logoStroke = for_ (move (Vec2 30 30) haskellLogo) (\closedPath -> do
    hsva 230 0.7 0.5 1
    renderPolygon closedPath
    setLineWidth 2
    stroke)

background :: Render ()
background = do
    rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
    hsva 275 0.7 0.5 0.1
    fill

randomPoints :: Render ()
randomPoints = replicateM_ 10000 $ do
    let range = (Vec2 0 0, Vec2 (fromIntegral picWidth - 1) (fromIntegral picHeight - 1))
    p@(Vec2 x y) <- liftIO (randomRIO range)
    let inLogo = any (\poly -> pointInPolygon p poly) (move (Vec2 30 30) haskellLogo)

    newPath
    arc x y 5 0 (2*pi)
    if inLogo
        then do hsva 0 0.7 0.5 0.1
                fill
        else pure ()




angledLineDraw :: Vec2 -> Double -> Double -> Render ()
angledLineDraw start angle len = do
    let Vec2 x y = start
    moveTo x y
    lineTo (x + len * cos angle) (y + len * sin angle)

scanlines = do
    let diagonalLine start = do
            newPath
            angledLineDraw start (pi/4) 1000
            closePath
    for_ (move (Vec2 30 30) haskellLogo) (\polygon -> do
        renderPolygon polygon
        clip
        for_ [-1000,-995..1000] (\x -> do
            diagonalLine (Vec2 x 0)
            setLineWidth 1
            stroke )
        resetClip )




renderPolygon :: Polygon -> Render ()
renderPolygon (Polygon []) = pure ()
renderPolygon (Polygon (Vec2 x y : vecs)) = do
    newPath
    moveTo x y
    for_ vecs (\(Vec2 x' y') -> lineTo x' y')
    closePath

haskellLogo :: [Polygon]
haskellLogo = [left, lambda, upper, lower]
  where
    left   = Polygon [Vec2 0 340.15625, Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625, Vec2 0 340.15625]
    lambda = Polygon [Vec2 113.386719 340.15625, Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625, Vec2 113.386719 340.15625]
    upper  = Polygon [Vec2 387.402344 240.945312, Vec2 349.609375 184.253906, Vec2 481.890625 184.25, Vec2 481.890625 240.945312, Vec2 387.402344 240.945312]
    lower  = Polygon [Vec2 330.710938 155.90625, Vec2 292.914062 99.214844, Vec2 481.890625 99.210938, Vec2 481.890625 155.90625, Vec2 330.710938 155.90625]
