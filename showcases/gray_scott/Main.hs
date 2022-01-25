{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map.Strict as M
import qualified Graphics.Rendering.Cairo as Cairo
import Data.Maybe (fromMaybe)
import System.Random.MWC (create)

import Draw
import Geometry
import Sampling
import Text.Printf (printf)

picWidth, picHeight :: Num a => a
picWidth = 200
picHeight = 200

main :: IO ()
main = do
    gen <- Cairo.liftIO create
    seeds <- Cairo.liftIO $ poissonDisc PoissonDisc
        { width = picWidth
        , height = picHeight
        , k = 4
        , radius = 100
        , .. }
    let grayScottProcess = grayScott GS
            { feedRateU = 0.04
            , killRateV = 0.1
            , diffusionRateU = 0.2
            , diffusionRateV = 0.1
            , width = picWidth
            , height = picHeight }
        initialState = M.fromList
            [ (p, (1 - v, v))
            | x <- [0..picWidth]
            , y <- [0..picHeight]
            , let p = Vec2 x y
            , let v = sum ((\q -> exp (- 0.5 * normSquare (p -. q))) <$> seeds)
            ]
        frames = scanl (flip ($)) initialState (replicate 5000 grayScottProcess)
    for_ (zip [0 :: Int ..] frames) $ \(index, frame) -> withSurfaceAuto (printf "out/gray_scott_%06i.png" index) picWidth picHeight (renderDrawing frame)
  where
    renderDrawing grid surface = Cairo.renderWith surface $ do
        cairoScope (setColor white >> Cairo.paint)
        drawing grid

drawing :: M.Map Vec2 (Double, Double) -> Cairo.Render ()
drawing grid = for_ [ Vec2 x y | x <- [0, 1..picWidth], y <- [0, 1..picHeight] ] $ \p@(Vec2 x y) -> do
    Cairo.rectangle x y 1 1
    setColor (hsv 0 0 (fst (grid ! p)))
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
(!) grid (Vec2 x y) = fromMaybe (1, 0) (grid M.!? p)
  where p = Vec2 (fromIntegral (round x `mod` picWidth)) (fromIntegral (round y `mod` picHeight))

grayScott :: GrayScott -> M.Map Vec2 (Double, Double) -> M.Map Vec2 (Double, Double)
grayScott GS{..} grid = M.mapWithKey grayScottStep grid
  where
    grayScottStep p (u0, v0) = (u0 + deltaU, v0 + deltaV)
      where
        u q = fst (grid ! q)
        v q = snd (grid ! q)
        deltaU = diffusionRateU * laplace u p - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplace v p + u0 * v0^2 - killRateV * v0
        laplace f (Vec2 x y) = f (Vec2 (x+1) y) + f (Vec2 (x-1) y) + f (Vec2 x (y+1)) + f (Vec2 x (y-1)) - 4 * f (Vec2 x y)
