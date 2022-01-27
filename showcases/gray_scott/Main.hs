{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Comonad
import System.Random.MWC (create)
import Text.Printf (printf)
import qualified Codec.Picture as P
import qualified Data.Vector.Storable as V

import Draw
import Geometry
import Sampling
import Zipper

picWidth, picHeight :: Num a => a
picWidth = 100
picHeight = 100

main :: IO ()
main = do
    gen <- create
    let width = picWidth
        height = picHeight
        k = 4

    seeds <- poissonDisc PoissonDisc { radius = 100, .. }

    let diffusionRate = 0.1
        initialState = planeFromList
            [ row
            | y <- [0..picHeight - 1]
            , let row =
                    [ (1 - v, v)
                    | x <- [0..picWidth - 1]
                    , let p = Vec2 x y
                    , let v = sum ((\q -> exp (- 0.005 / diffusionRate * normSquare (p -. q))) <$> seeds)
                    ]
            ]

        grayScottProcess = grayScott GS
            { feedRateU = 0.029
            , killRateV = 0.057
            , diffusionRateU = 2 * diffusionRate
            , diffusionRateV = diffusionRate
            , width = picWidth
            , height = picHeight }
        frames = take 500 (iterate (grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess) initialState)
    for_ (zip [0 :: Int ..] frames) $ \(index, grid) -> do
        let file = printf "out/gray_scott_%06i.png" index
        P.writePng file (renderImage grid)

renderImage :: Grid -> P.Image P.Pixel8
renderImage grid = P.Image picWidth picHeight (V.concat (V.fromList <$> planeToList (renderPixel <$> grid)))
  where renderPixel (u, _v) = round (u * 255)

type Grid = Plane (Double, Double)

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
    grayScottStep grid = uv0 +. (deltaU, deltaV)
      where
        uv0@(u0, v0) = extract grid
        deltaU = diffusionRateU * laplaceU - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplaceV + u0 * v0^2 - (feedRateU + killRateV) * v0
        (uv11, uv12, uv13, uv21, uv22, uv23, uv31, uv32, uv33) = neighbours grid
        (laplaceU, laplaceV) = (uv11 +. 2*.uv12 +. uv13 +. 2*.uv21 -. 12*.uv22 +. 2*. uv23 +. uv31 +. 2*.uv32 +. uv33) /. 4

neighbours :: Plane a -> (a, a, a, a, a, a, a, a, a)
neighbours grid = (x11, x12, x13, x21, x22, x23, x31, x32, x33)
  where
    row1 = moveUp <$> row2
    row2 = [moveLeft grid, grid, moveRight grid]
    row3 = moveDown <$> row2
    [x11, x12, x13, x21, x22, x23, x31, x32, x33] = extract <$> concat [row1, row2, row3]
