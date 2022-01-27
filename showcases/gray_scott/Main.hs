{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Random.MWC (create)
import Text.Printf (printf)
import qualified Codec.Picture as P
import qualified Data.Vector.Storable as V

import Draw
import Geometry
import Plane
import Sampling

picWidth, picHeight :: Num a => a
picWidth = 200
picHeight = 200

main :: IO ()
main = do
    gen <- create
    let width = picWidth
        height = picHeight
        k = 4

    seeds <- poissonDisc PoissonDisc { radius = 300, .. }

    let diffusionRate = 0.1
        initialState = planeFromList
            [ row
            | y <- [0..picHeight - 1]
            , let row =
                    [ (1 - v, v)
                    | x <- [0..picWidth - 1]
                    , let p = Vec2 x y
                    , let v = sum ((\q -> exp (- 0.0005 / diffusionRate * normSquare (p -. q))) <$> seeds)
                    ]
            ]

        grayScottProcess = grayScott GS
            { feedRateU = 0.029
            , killRateV = 0.057
            , diffusionRateU = 2 * diffusionRate
            , diffusionRateV = diffusionRate
            , width = picWidth
            , height = picHeight }
        frames = take 100 (iterate (grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess) initialState)
    for_ (zip [0 :: Int ..] frames) $ \(index, grid) -> do
        let file = printf "out/gray_scott_%06i.png" index
        P.writePng file (renderImage grid)

renderImage :: Grid -> P.Image P.Pixel8
renderImage grid = P.Image picWidth picHeight (V.convert (items (mapPlane renderPixel grid)))
  where renderPixel (u, _v) = round $ max 0 $ min 255 $ u * 255

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
grayScott GS{..} = mapNeighbours grayScottStep
  where
    grayScottStep (uv11, uv12, uv13, uv21, uv22, uv23, uv31, uv32, uv33) = uv22 +. (deltaU, deltaV)
      where
        (u0, v0) = uv22
        deltaU = diffusionRateU * laplaceU - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplaceV + u0 * v0^2 - (feedRateU + killRateV) * v0
        (laplaceU, laplaceV) = (uv11 +. 2*.uv12 +. uv13 +. 2*.uv21 -. 12*.uv22 +. 2*. uv23 +. uv31 +. 2*.uv32 +. uv33) /. 4
