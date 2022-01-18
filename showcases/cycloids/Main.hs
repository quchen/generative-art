{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Foldable (for_)
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
    cairoScope (setColor colorBackground >> Cairo.paint)
    Cairo.translate (picWidth / 2) (picHeight / 2)

    let --curve t = epitrochoid Trochoid { baseRadius = 350, rotorRadius = 50, pencilRadius = 2*t } t
        curve t = hypotrochoid Trochoid { baseRadius = t+200, rotorRadius = 0.901*(t+200), pencilRadius = -0.2*t + 1000 } t

    moveToVec (curve 0)
    for_ [0, 0.01 .. 3000] $ lineToVec . curve

    setColor colorForeground
    Cairo.stroke
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

colorBackground, colorForeground :: Color Double
colorBackground = parseRGBHex "#073642"
colorForeground = parseRGBHex "#586e75"
