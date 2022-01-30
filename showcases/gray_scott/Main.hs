{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Random.MWC (create)
import Text.Printf (printf)
import qualified Codec.Picture as P
import qualified Codec.Picture.Extra as P
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U

import Draw
import Geometry hiding (Grid)
import Plane
import Sampling

picWidth, picHeight :: Num a => a
picWidth = 480
picHeight = 270

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
                    [ (u, v, 0, 0)
                    | x <- [0..picWidth - 1]
                    , let p = Vec2 x y
                    , let u = 1 - sum ((\q -> exp (- 0.05 * diffusionRate * normSquare (p -. q))) <$> seeds)
                    , let v = sum ((\q -> exp (- 0.05 * diffusionRate * normSquare (p +. Vec2 0 10 -. q))) <$> seeds)
                    ]
            ]

        frames = take 100 (iterate (grayScott 5 params) initialState)

    for_ (zip [0 :: Int ..] frames) $ \(index, grid) -> do
        P.writePng (printf "out/gray_scott_%06i.png" index) (renderImageGradient grid)
        P.writePng (printf "out/gray_scott_%06i_experimental.png" index) $ P.beside
            [ P.below
                [ fromGrayscale $ renderImageGrayscale (mapPlane (\(u, _, _, _) -> 1-u) grid)
                , fromGrayscale $ renderImageGrayscale (mapPlane (\(_, v, _, _) -> v) grid)
                , renderImageGradient' (\(_, _, du, dv) -> tanh (200*(du+dv)) *. (-1, 0, 1)) grid
                ]
            , P.below
                [ renderImageGradient' (\(_, _, du, _) -> tanh (400*du) *. (-1, 0, 1)) grid
                , renderImageGradient' (\(_, _, _, dv) -> tanh (400*dv) *. (1, 0, -1)) grid
                , renderImageGradient' (\(_, _, du, dv) -> tanh (200*(du-dv)) *. (-1, 0, 1)) grid
                ]
            , P.below
                [ fromGrayscale $ renderImageGrayscale (mapPlane (\(u, v, _, _) -> 10*u*v*v) grid)
                , renderImageGradient' (\(u, v, du, dv) -> tanh (400 * max 0 du) *. (0, 0, 0.5) +. tanh (-400 * min 0 dv) *. (0.7, 0, -0.1) +. tanh (400 * max 0 dv) *. (0.4, 0.1, 0) +. gradient (22*u*v*v)) grid
                , renderImageGradient' (\(u, v, du, dv) -> tanh (200 * du) *. (0.75, 0, 0.5) -. tanh (200 * dv) *. (1, 0, 0) +. gradient (22*u*v*v)) grid
                ]
            ]

fromGrayscale :: P.Image P.Pixel8 -> P.Image P.PixelRGB8
fromGrayscale (P.Image w h pixels) = P.Image w h (V.concatMap (\px -> V.fromList [px, px, px]) pixels)

renderImageGrayscale :: Plane Double -> P.Image P.Pixel8
renderImageGrayscale grid = P.Image picWidth picHeight (V.convert (items (mapPlane renderPixel grid)))
  where renderPixel = round . max 0 . min 255 . (* 255)

renderImageGradient' :: ((Double, Double, Double, Double) -> (Double, Double, Double)) -> Grid -> P.Image P.PixelRGB8
renderImageGradient' f grid = P.Image picWidth picHeight (V.convert $ U.concatMap renderPixel (items grid))
  where
    renderPixel uv = let (r, g, b) = f uv in U.fromList [pixel8 r, pixel8 g, pixel8 b]
    pixel8 = round . clamp 0 255 . (* 255)

renderImageGradient :: Grid -> P.Image P.PixelRGB8
renderImageGradient grid = P.Image picWidth picHeight $ V.concatMap renderPixel $ V.convert $ items $ mapPlane (\(_, v, _, _) -> v) grid
  where
    renderPixel v = let (r, g, b) = gradient (2.4*v) in V.fromList [pixel8 r, pixel8 g, pixel8 b]
    pixel8 = round . clamp 0 255 . (* 255)

gradient :: Double -> (Double, Double, Double)
gradient v
    | v < 0.4 = interpolate (2.5*v) color1 color0
    | v < 1.0 = interpolate (1.667*(v-0.4)) color2 color1
    | otherwise = interpolate (v-1) color3 color2
  where
    color0, color1, color2, color3 :: (Double, Double, Double)
    color0 = (0, 0, 0.1)
    color1 = 0.5 *. (0.1, 0.25, 0.5)
    color2 = (0.1, 0.9, 0.1)
    color3 = (255, 0, 0)
    interpolate a c1 c2 = a *. c1 +. (1-a) *. c2

clamp :: (Ord a, Num a) => a -> a -> a -> a
clamp lower upper = max lower . min upper

type Grid = Plane (Double, Double, Double, Double)

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
grayScott steps GS{..} = repeatF steps (mapNeighbours grayScottStep)
  where
    grayScottStep (uv11, uv12, uv13, uv21, uv22, uv23, uv31, uv32, uv33) = (u0, v0, deltaU, deltaV) +. step *. (deltaU, deltaV, 0, 0)
      where
        (u0, v0, _, _) = uv22
        deltaU = diffusionRateU * laplaceU - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplaceV + u0 * v0^2 - (feedRateU + killRateV) * v0
        (laplaceU, laplaceV, _, _) = (uv11 +. 2*.uv12 +. uv13 +. 2*.uv21 -. 12*.uv22 +. 2*. uv23 +. uv31 +. 2*.uv32 +. uv33) /. 4

    repeatF :: Int -> (a -> a) -> a -> a
    repeatF 0 _ = id
    repeatF n f = f . repeatF (n-1) f
