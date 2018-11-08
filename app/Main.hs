module Main where



import Graphics.Rendering.Cairo
import Data.Foldable
import           Data.Time.Clock.POSIX
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import Data.Semigroup

import Lib



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
    for_ (move (Vec2 30 30) haskellLogo) (\closedPath -> do
        hsva 230 0.7 0.5 1
        renderPolygon closedPath
        setLineWidth 3
        stroke)
    scanlines

background :: Render ()
background = do
    rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
    hsva 275 0.7 0.5 0.1
    fill

angledLineDraw x y angle len = do
    moveTo x y
    lineTo (x + len * cos angle) (y + len * sin angle)

scanlines = do
    let diagonalLine x y = do
            newPath
            angledLineDraw x y (pi / 4) 1000
            closePath
    for_ (move (Vec2 30 30) haskellLogo) (\polygon -> do
        renderPolygon polygon
        clip
        for_ [-1000,-995..1000] (\x -> do
            diagonalLine x 0
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
