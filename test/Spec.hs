{-# LANGUAGE RecordWildCards #-}

import Graphics.Rendering.Cairo
import System.Random
import Data.Foldable
import Control.Monad
import Data.Time.Clock.POSIX
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Semigroup

import Geometry
import Draw



main :: IO ()
main = testIntersectionLL


testIntersectionLL = renderPng "out/test1.png" (do
    testVirtual
    testVirtualL
    testVirtualR
    testReal1
    testReal2
    )
  where
    testVirtual
      = testDraw (angledLine (Vec2 50 190) (Angle ( pi/6)) (Distance 100))
                 (angledLine (Vec2 50 300) (Angle (-pi/6)) (Distance 100))
    testVirtualL
      = testDraw (angledLine (Vec2 300 180) (Angle      0) (Distance 100))
                 (angledLine (Vec2 350 200) (Angle (pi/2)) (Distance 100))
    testVirtualR
      = testDraw (angledLine (Vec2 420 230) (Angle      0) (Distance 100))
                 (angledLine (Vec2 400 200) (Angle (pi/2)) (Distance 100))
    testReal1
      = testDraw (Line (Vec2 10  10) (Vec2 220 190))
                 (Line (Vec2 270 50) (Vec2  30 160))
    testReal2
      = testDraw (move (Vec2 (picWidth/2) 10) (Line (Vec2 0   0) (Vec2 120 120)))
                 (move (Vec2 (picWidth/2) 10) (Line (Vec2 120 0) (Vec2 0   120)))

    testDraw line1 line2 = do
        let (point, angle, ty) = intersectionLL line1 line2

        newPath

        setLineWidth 1
        hsva 0 1 0.7 1
        arrowSketch line1
        stroke
        hsva 60 1 0.7 1
        arrowSketch line2
        stroke

        hsva 120 1 0.7 1
        circleSketch point (Distance 3)
        fill

        hsva 180 1 0.7 1
        arcSketch point (Distance 10) (angleL line1) (angleL line2)
        stroke

        do let fontSize = 10
           let Vec2 x y = point `addVec2` Vec2 15 15
           hsva 0 0 0 1
           moveTo x y
           setFontSize fontSize
           let Angle alpha = angle
               angleDeg = round (alpha / (2 * pi) * 360) :: Int
           showText (show ty ++ ", " ++ show angleDeg ++ "°")

        closePath

arrowSketch :: Line -> Render ()
arrowSketch line = do
    lineSketch line
    let Line start end = line
        Angle rawAngle = angleL line
    lineSketch (angledLine end (Angle (rawAngle + pi - 0.5)) (Distance 10))
    lineSketch (angledLine end (Angle (rawAngle + pi + 0.5)) (Distance 10))

picWidth, picHeight :: Num a => a
picWidth = 640
picHeight = 480

renderPng filename drawing = do
    surface <- createImageSurface FormatARGB32 picWidth picHeight
    renderWith surface (do
        background
        drawing
        )
    surfaceWriteToPNG surface filename

background :: Render ()
background = do
    newPath
    rectangle 0 0 (fromIntegral picWidth) (fromIntegral picHeight)
    hsva 0 0 0 1
    setLineWidth 1
    stroke

hsva h s v a = setSourceRGBA channelRed channelGreen channelBlue a
    where RGB{..} = hsv h s v
