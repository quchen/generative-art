{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (when)
import qualified Graphics.Rendering.Cairo as Cairo

import Draw
import Geometry


picHeight, picWidth :: Num a => a
picWidth = 2560
picHeight = 2560

scaleFactor :: Double
scaleFactor = 1

main :: IO ()
main = withSurfaceAuto "out/cycloids.png" scaledWidth scaledHeight $ \surface -> Cairo.renderWith surface $ do
    Cairo.scale scaleFactor scaleFactor
    cairoScope (setColor colorBackground2 >> Cairo.paint)

    let curve k t = hypotrochoid Trochoid
            { baseRadius = t+800
            , rotorRadius = (0.9 + 0.0001*k)*(t+800)
            , pencilRadius = -0.2*t + 1800 }
            (t*0.5)

    for_ [0..16] $ \k -> do
        cairoScope $ do
            Cairo.scale 0.25 0.25
            Cairo.rectangle 240 240 2080 2080
            cairoScope $ do
                setColor colorForeground2
                Cairo.setLineWidth 20
                Cairo.strokePreserve
            cairoScope $ do
                setColor colorBackground1
                Cairo.fillPreserve
            Cairo.clip

            Cairo.translate (picWidth / 2) (picHeight / 2)
            moveToVec (curve (fromIntegral k) 0)
            for_ [0, 0.2 .. 5700] $ lineToVec . curve (fromIntegral k)

            setColor colorForeground1
            Cairo.stroke

        Cairo.translate (picWidth/4) 0
        when (k `mod` 4 == 3) $ Cairo.translate (-picWidth) (picHeight / 4)

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
    order0 = polar (rad t) (Distance (baseRadius + rotorRadius))
    order1 = polar (rad $ (baseRadius + rotorRadius) / rotorRadius * t) (Distance 1)

hypotrochoid :: Trochoid -> Double -> Vec2
hypotrochoid Trochoid{..} t =  order0 +. pencilRadius *. order1
  where
    order0 = polar (rad t) (Distance $ baseRadius - rotorRadius)
    order1 = polar (rad $ (rotorRadius - baseRadius) / rotorRadius * t) (Distance 1)

colorBackground1, colorForeground1, colorBackground2, colorForeground2 :: Color Double
colorBackground1 = parseRGBHex "#002b36"
colorBackground2 = parseRGBHex "#073642"
colorForeground1 = parseRGBHex "#586e75"
colorForeground2 = parseRGBHex "#657b83"
