{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Comonad
import System.Random.MWC (create)
import Text.Printf (printf)
import qualified Codec.Picture as P
import qualified Data.Vector.Storable as V

import Draw
import Geometry hiding (Grid)
import Sampling
import Zipper

picWidth, picHeight :: Num a => a
picWidth = 200
picHeight = 200

main :: IO ()
main = do
    gen <- create
    seeds <- poissonDisc PoissonDisc
        { width = picWidth
        , height = picHeight
        , k = 4
        , radius = 100
        , .. }
    let grayScottProcess = grayScott GS
            { feedRateU = 0.029
            , killRateV = 0.057
            , diffusionRateU = 0.04
            , diffusionRateV = 0.02
            , width = picWidth
            , height = picHeight }
        initialState = planeFromList
            [ row
            | y <- [0..picHeight - 1]
            , let row =
                    [ (1 - v, v)
                    | x <- [0..picWidth - 1]
                    , let p = Vec2 x y
                    , let v = sum ((\q -> exp (- 0.5 * normSquare (p -. q))) <$> seeds)
                    ]
            ]
        frames = take 1500 (iterate (grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess) initialState)
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
    grayScottStep grid = (u0 + deltaU, v0 + deltaV)
      where
        (u0, v0) = extract grid
        u f = fst (extract (f grid))
        v f = snd (extract (f grid))
        deltaU = diffusionRateU * laplace u - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplace v + u0 * v0^2 - (feedRateU + killRateV) * v0
        laplace f = f moveRight + f moveUp + f moveLeft + f moveDown - 4 * f id
