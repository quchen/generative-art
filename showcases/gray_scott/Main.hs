{-# LANGUAGE RecordWildCards #-}
module Main where

import Text.Printf (printf)
import qualified Codec.Picture as P
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U

import Draw
import Geometry hiding (Grid)
import Plane

-- | Settings that work well:
-- * 1080p rendering:   spatialResolution = 10, temporalResolution = 9, temporalResolutionWarmup = 10
-- * 540p rendering:    spatialResolution = 5,  temporalResolution = 3 (better quality with 5), temporalResolutionWarmup = 10
-- * Rapid prototyping: spatialResolution = 3,  temporalResolution = 2, temporalResolutionWarmup = 6
spatialResolution, temporalResolution, temporalResolutionWarmup :: Num a => a
spatialResolution = 3
temporalResolution = 2
temporalResolutionWarmup = 6

main :: IO ()
main = do

    let picWidth = 192 * spatialResolution
        picHeight = 108 * spatialResolution

    let seeds = [ Vec2 (picWidth/2) (picHeight/2) ]

    let diffusionRate = 0.004
        params = GS
            { feedRateU = 0.029
            , killRateV = 0.057
            , diffusionRateU = 2 * diffusionRate * spatialResolution^2
            , diffusionRateV = diffusionRate * spatialResolution^2
            , step = 10 / temporalResolution
            , width = picWidth
            , height = picHeight }
        warmup = grayScott (10*temporalResolutionWarmup) params { step = 10/temporalResolutionWarmup }
        initialState = warmup $ planeFromList
            [ row
            | y <- [0..picHeight - 1]
            , let row =
                    [ (u, v, 0, 0)
                    | x <- [0..picWidth - 1]
                    , let p = Vec2 x y
                    , let u = 1 - sum ((\q -> exp (- 0.125 / spatialResolution^2 * normSquare (p -. q))) <$> seeds)
                    , let v = sum ((\q -> exp (- 0.125 / spatialResolution^2 * normSquare (p +. Vec2 0 (2*spatialResolution) -. q))) <$> seeds)
                    ]
            ]

        frames = takeWhile ((< 2000) . fst) (iterate (\(t, state) -> (t + 1, grayScott temporalResolution (scene (fromIntegral t) params) state)) (0 :: Int, initialState))

    for_ frames $ \(index, grid) -> do
        P.writePng (printf "out/gray_scott_%06i.png" index) (renderImageColor (colorFront +. colorTrail +. colorReaction) grid)
        P.writePng (printf "out/uv_gray_scott_%06i.png" index) (renderImageColor (\(u, v, _, _) -> (1-u-v, v, u)) grid)

scene :: Double -> GrayScott -> GrayScott
scene t baseParams =
    let scene1 = baseParams
        t1 :: GrayScott -> GrayScott -> GrayScott
        t1 = transition 400 10 t
        scene2 = baseParams { killRateV = 0.060, step = 2 / temporalResolution }
        t2 = transition 450 100 t
        scene3 = baseParams { killRateV = 0.062, step = 2 / temporalResolution }
        t3 = transition 600 50 t
        scene4 = baseParams { killRateV = 0.062, step = 5 / temporalResolution }
        t4 = transition 1000 50 t
        scene5 = baseParams { killRateV = 0.062, feedRateU = 0.045 }
        t5 = linearTransition 1000 2000 t
    in  scene1 `t1` scene2 `t2` scene3 `t3` scene4 `t4` scene5 `t5` scene1

transition :: Double -> Double -> Double -> GrayScott -> GrayScott -> GrayScott
transition t0 duration = \t a b -> a
    { feedRateU = sigmoid (t0-t) * feedRateU a + sigmoid (t-t0) * feedRateU b
    , killRateV = sigmoid (t0-t) * killRateV a + sigmoid (t-t0) * killRateV b
    , diffusionRateU = sigmoid (t0-t) * diffusionRateU a + sigmoid (t-t0) * diffusionRateU b
    , diffusionRateV = sigmoid (t0-t) * diffusionRateV a + sigmoid (t-t0) * diffusionRateV b
    , step = sigmoid (t0-t) * step a + sigmoid (t-t0) * step b
    }
  where sigmoid t = 0.5 * (1 + tanh (2*pi*t/duration))

linearTransition :: Double -> Double -> Double -> GrayScott -> GrayScott -> GrayScott
linearTransition t0 t1 t a b
    | t < t0 = a
    | t < t1 = a
        { feedRateU      = (t1-t) / deltaT * feedRateU a      + (t-t0) / deltaT * feedRateU b
        , killRateV      = (t1-t) / deltaT * killRateV a      + (t-t0) / deltaT * killRateV b
        , diffusionRateU = (t1-t) / deltaT * diffusionRateU a + (t-t0) / deltaT * diffusionRateU b
        , diffusionRateV = (t1-t) / deltaT * diffusionRateV a + (t-t0) / deltaT * diffusionRateV b
        , step           = (t1-t) / deltaT * step a           + (t-t0) / deltaT * step b
        }
    | otherwise = b
  where deltaT = t1 - t0

renderImageColor :: ((Double, Double, Double, Double) -> (Double, Double, Double)) -> Grid -> P.Image P.PixelRGB8
renderImageColor f Plane{..} = P.Image sizeX sizeY (V.convert $ U.concatMap renderPixel items)
  where
    renderPixel uv = let (r, g, b) = f uv in U.fromList [pixel8 r, pixel8 g, pixel8 b]
    pixel8 = round . clamp 0 255 . (* 255)

colorFront :: (Double, Double, Double, Double) -> (Double, Double, Double)
colorFront (_, _, _, dv) = tanh (400 * max 0 dv) *. (0.4, 0.1, 0)

colorTrail :: (Double, Double, Double, Double) -> (Double, Double, Double)
colorTrail (_, _, du, dv) = tanh (400 * max 0 du) *. (0, 0, 0.5) +. tanh (-400 * min 0 dv) *. (0.7, 0, -0.1)

colorReaction :: (Double, Double, Double, Double) -> (Double, Double, Double)
colorReaction (u, v, _, _) = case tanh (30 * u * v * v) of
    x | x < 0.5   -> interpolate (2*x) color1 color0
      | otherwise -> interpolate (2*(x-0.5)) color2 color1
  where
    color0, color1, color2 :: (Double, Double, Double)
    color0 = (0, 0, 0.1)
    color1 = (0.1, 0.25, 0.5)
    color2 = (0.1, 0.9, 0.1)
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
    } deriving (Show)

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
