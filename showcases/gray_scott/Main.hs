{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map.Strict as M
import qualified Graphics.Rendering.Cairo as Cairo
import Data.Maybe (fromMaybe)

import Draw
import Geometry
import Graphics.Rendering.Cairo (RectangleInt(y))

picWidth, picHeight :: Num a => a
picWidth = 50
picHeight = 50

main :: IO ()
main = withSurfaceAuto "out/gray_scott.png" picWidth picHeight renderDrawing
  where
    renderDrawing surface = Cairo.renderWith surface $ do
        cairoScope (setColor white >> Cairo.paint)
        drawing

drawing :: Cairo.Render ()
drawing = do
    let grayScottProcess = grayScott GS
            { feedRateU = 0.04
            , killRateV = 0.1
            , diffusionRateU = 0.2
            , diffusionRateV = 0.1
            , width = picWidth
            , height = picHeight }
        origin = Vec2 (picWidth/2) (picHeight/2)
        p1 = Vec2 (picWidth/2.9) (picHeight/2.8)
        p2 = Vec2 (picWidth/2.6) (picHeight/1.2)
        initialState = M.fromList
            [ (p, (u, 1-u))
            | x <- [0..picWidth]
            , y <- [0..picHeight]
            , let p = Vec2 x y
            , let u = 1 - exp (- 0.5 * normSquare (p -. origin)) - exp (- normSquare (p -. p1)) - exp (- normSquare (p -. p2))
            ]
        finalState = foldr ($) initialState (replicate 400 grayScottProcess)
    for_ [ Vec2 x y | x <- [0, 1..picWidth], y <- [0, 1..picHeight] ] $ \p@(Vec2 x y) -> do
        Cairo.rectangle x y 1 1
        setColor (hsv 0 0 (fst (finalState ! p)))
        Cairo.fill

data GrayScott = GS
    { feedRateU :: Double
    , killRateV :: Double
    , diffusionRateU :: Double
    , diffusionRateV :: Double
    , width :: Double
    , height :: Double
    }

(!) :: M.Map Vec2 (Double, Double) -> Vec2 -> (Double, Double)
(!) grid p = fromMaybe (1, 0) (grid M.!? p)

grayScott :: GrayScott -> M.Map Vec2 (Double, Double) -> M.Map Vec2 (Double, Double)
grayScott GS{..} grid = foldl' (\g p -> M.insert p (grayScottStep p) g) grid [ Vec2 x y | x <- [0..width], y <- [0..height] ]
  where
    grayScottStep p = (u0 + deltaU, v0 + deltaV)
      where
        u q = fst (grid ! q)
        v q = snd (grid ! q)
        (u0, v0) = grid ! p
        deltaU = diffusionRateU * laplace u p - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplace v p + u0 * v0^2 - killRateV * v0
        laplace f (Vec2 x y) = (f (Vec2 (x+1) y) + f (Vec2 (x-1) y) - 2 * f (Vec2 x y)) + (f (Vec2 x (y+1)) + f (Vec2 x (y-1)) - 2 * f (Vec2 x y))
