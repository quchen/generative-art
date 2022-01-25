{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Comonad
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC (create)
import Text.Printf (printf)

import Draw
import Geometry hiding (Grid)
import Sampling
import Zipper

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
        initialState = planeFromList
            [ row
            | y <- [0..picHeight]
            , let row =
                    [ (1 - v, v)
                    | x <- [0..picWidth]
                    , let p = Vec2 x y
                    , let v = sum ((\q -> exp (- 0.5 * normSquare (p -. q))) <$> seeds)
                    ]
            ]
        frames = take 1000 (iterate (grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess) initialState)
    for_ (zip [0 :: Int ..] frames) $ \(index, frame) -> withSurfaceAuto (printf "out/gray_scott_%06i.png" index) picWidth picHeight (renderDrawing frame)
  where
    renderDrawing grid surface = Cairo.renderWith surface $ do
        cairoScope (setColor white >> Cairo.paint)
        drawing grid

type Grid = Plane (Double, Double)

drawing :: Grid -> Cairo.Render ()
drawing grid = for_ (zip [1..] (planeToList grid)) $ \(y, row) ->
    for_ (zip [1..] row) $ \(x, (u, _v)) -> do
        Cairo.rectangle x y 1 1
        setColor (hsv 0 0 u)
        Cairo.fill

data GrayScott = GS
    { feedRateU :: Double
    , killRateV :: Double
    , diffusionRateU :: Double
    , diffusionRateV :: Double
    , width :: Double
    , height :: Double
    }

grayScott :: GrayScott -> Grid -> Grid
grayScott GS{..} = extend grayScottStep
  where
    grayScottStep grid = (u0 + deltaU, v0 + deltaV)
      where
        (u0, v0) = extract grid
        u f = fst (extract (f grid))
        v f = snd (extract (f grid))
        deltaU = diffusionRateU * laplace u - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplace v + u0 * v0^2 - killRateV * v0
        laplace f = f moveRight + f moveUp + f moveLeft + f moveDown - 4 * f id
