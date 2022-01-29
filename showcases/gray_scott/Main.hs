{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Random.MWC (create)
import Text.Printf (printf)
import qualified Codec.Picture as P
import qualified Data.Vector.Storable as V

import Draw
import Geometry hiding (Grid)
import Plane
import Sampling
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.SRGB (toSRGB)

picWidth, picHeight :: Num a => a
picWidth = 960
picHeight = 540

main :: IO ()
main = do
    gen <- create
    let width = picWidth
        height = picHeight
        k = 4

    seeds <- poissonDisc PoissonDisc { radius = 300, .. }

    let diffusionRate = 0.1
        params = GS
            { feedRateU = 0.029
            , killRateV = 0.057
            , diffusionRateU = 2 * diffusionRate
            , diffusionRateV = diffusionRate
            , step = 2
            , width = picWidth
            , height = picHeight }
        warmup = grayScott 100 params { step = 1 }
        initialState = warmup $ planeFromList
            [ row
            | y <- [0..picHeight - 1]
            , let row =
                    [ (u, v)
                    | x <- [0..picWidth - 1]
                    , let p = Vec2 x y
                    , let u = 1 - sum ((\q -> exp (- 0.05 * diffusionRate * normSquare (p -. q))) <$> seeds)
                    , let v = sum ((\q -> exp (- 0.05 * diffusionRate * normSquare (p +. Vec2 0 10 -. q))) <$> seeds)
                    ]
            ]

        frames = take 100 (iterate (grayScott 5 params) initialState)
    for_ (zip [0 :: Int ..] frames) $ \(index, grid) -> do
        let file = printf "out/gray_scott_%06i.png" index
        P.writePng file (renderImage grid)

renderImage :: Grid -> P.Image P.PixelRGB8
renderImage grid = P.Image picWidth picHeight $ V.concatMap renderPixel $ V.convert $ items $ mapPlane snd grid
  where renderPixel v = let P.PixelRGB8 r g b = gradient v in V.fromList [r, g, b]

gradient :: Double -> P.PixelRGB8
gradient v
    | v < 0.25 = toPixel $ blend (4*v) color1 color0
    | v < 0.5 = toPixel $ blend (4*(v-0.25)) color2 color1
    | otherwise = toPixel $ blend (2*(v-0.5)) color3 color2
  where
    color0 = hsv 240 0.9 0.1
    color1 = hsv 210 0.8 0.5
    color2 = hsv 120 0.8 0.9
    color3 = hsv 0 0.8 0.9
    toPixel = uncurryRGB (\r g b -> P.PixelRGB8 (pixel8 r) (pixel8 g) (pixel8 b)) . toSRGB
    pixel8 = round . clamp 0 255 . (* 255)

clamp :: (Ord a, Num a) => a -> a -> a -> a
clamp lower upper = max lower . min upper

type Grid = Plane (Double, Double)

data GrayScott = GS
    { feedRateU :: Double
    , killRateV :: Double
    , diffusionRateU :: Double
    , diffusionRateV :: Double
    , step :: Double
    , width :: Double
    , height :: Double
    }

grayScott :: Int -> GrayScott -> Grid -> Grid
grayScott n GS{..} = repeatF n (mapNeighbours grayScottStep)
  where
    grayScottStep (uv11, uv12, uv13, uv21, uv22, uv23, uv31, uv32, uv33) = uv22 +. step *. (deltaU, deltaV)
      where
        (u0, v0) = uv22
        deltaU = diffusionRateU * laplaceU - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplaceV + u0 * v0^2 - (feedRateU + killRateV) * v0
        (laplaceU, laplaceV) = (uv11 +. 2*.uv12 +. uv13 +. 2*.uv21 -. 12*.uv22 +. 2*. uv23 +. uv31 +. 2*.uv32 +. uv33) /. 4

    repeatF :: Int -> (a -> a) -> a -> a
    repeatF 0 _ = id
    repeatF n f = f . repeatF (n-1) f
