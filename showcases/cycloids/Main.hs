module Main where

import Control.Monad (when)
import Graphics.Rendering.Cairo as C

import Draw
import Geometry as G


picHeight, picWidth :: Num a => a
picWidth = 2560
picHeight = 2560

scaleFactor :: Double
scaleFactor = 1

main :: IO ()
main = withSurfaceAuto "out/cycloids.png" scaledWidth scaledHeight $ \surface -> C.renderWith surface $ do
    C.scale scaleFactor scaleFactor
    cairoScope (setColor colorBackground2 >> C.paint)

    let curve k t = hypotrochoid Trochoid
            { baseRadius = t+800
            , rotorRadius = (0.9 + 0.0001*k)*(t+800)
            , pencilRadius = -0.2*t + 1800 }
            (t*0.5)

    for_ [0..16] $ \k -> do
        cairoScope $ do
            C.scale 0.25 0.25
            rectangle 240 240 2080 2080
            cairoScope $ do
                setColor colorForeground2
                setLineWidth 20
                strokePreserve
            cairoScope $ do
                setColor colorBackground1
                fillPreserve
            clip

            C.translate (picWidth / 2) (picHeight / 2)
            moveToVec (curve (fromIntegral k) 0)
            for_ [0, 0.2 .. 5700] $ lineToVec . curve (fromIntegral k)

            setColor colorForeground1
            stroke

        C.translate (picWidth/4) 0
        when (k `mod` 4 == 3) $ C.translate (-picWidth) (picHeight / 4)

  where
    scaledWidth = round (scaleFactor * picWidth)
    scaledHeight = round (scaleFactor * picHeight)


data Trochoid = Trochoid
    { baseRadius :: Double
    , rotorRadius :: Double
    , pencilRadius :: Double
    }

epitrochoid :: Trochoid -> Double -> Vec2
epitrochoid Trochoid{..} t = order0 -. pencilRadius *. order1
  where
    order0 = polar (rad t) (baseRadius + rotorRadius)
    order1 = polar (rad $ (baseRadius + rotorRadius) / rotorRadius * t) 1

hypotrochoid :: Trochoid -> Double -> Vec2
hypotrochoid Trochoid{..} t =  order0 +. pencilRadius *. order1
  where
    order0 = polar (rad t) (baseRadius - rotorRadius)
    order1 = polar (rad $ (rotorRadius - baseRadius) / rotorRadius * t) 1

colorBackground1, colorForeground1, colorBackground2, colorForeground2 :: Color Double
colorBackground1 = parseRgbHex "#002b36"
colorBackground2 = parseRgbHex "#073642"
colorForeground1 = parseRgbHex "#586e75"
colorForeground2 = parseRgbHex "#657b83"
