{-# LANGUAGE RecordWildCards #-}
module Main where

import Text.Printf (printf)
import qualified Codec.Picture as P
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U
import Math.Noise
import System.Environment (getArgs)

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
    args <- getArgs
    case args of
        [index, uvfile] -> do
            Right (P.ImageRGB8 uvimg) <- P.readPng uvfile
            let initialState = initialStateFromFile uvimg
                frames = simulation (read index) initialState
            writeOutput (tail frames)

        _otherwise ->
            writeOutput (simulation 0 initialStateFromScratch)
    
initialStateFromFile :: P.Image P.PixelRGB8 -> Grid
initialStateFromFile uvimg@(P.Image picWidth picHeight _) = planeFromList
    [ row
    | y <- [0..picHeight - 1]
    , let row =
            [ (fromIntegral u / 255, fromIntegral v / 255, 0, 0)
            | x <- [0..picWidth - 1]
            , let P.PixelRGB8 _ v u = P.pixelAt uvimg x y
            ]
    ]

initialStateFromScratch :: Grid
initialStateFromScratch = warmup $ planeFromList
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
  where
    picWidth = 192 * spatialResolution
    picHeight = 108 * spatialResolution
    seeds = [ Vec2 (picWidth/2) (picHeight/2) ]
    warmup = grayScott (10 * temporalResolutionWarmup) (scene 0) { step = 10/temporalResolutionWarmup }

simulation :: Int -> Grid -> [(Int, Grid)]
simulation t0 initialState = takeWhile ((< 4000) . fst) (iterate (\(t, state) -> (t + 1, grayScott temporalResolution (scene (fromIntegral t)) state)) (t0, initialState))

writeOutput :: [(Int, Grid)] -> IO ()
writeOutput frames = for_ frames $ \(index, grid) -> do
    P.writePng (printf "out/gray_scott_%06i.png" index) (renderImageColor (colorFront +. colorTrail +. colorReaction) grid)
    P.writePng (printf "out/uv_gray_scott_%06i.png" index) (renderImageColor (\(u, v, _, _) -> (1-u-v, v, u)) grid)

scene :: Double -> GrayScott
scene t = scene1 `t1` scene2 `t2` scene3 `t3` scene4 `t4` scene5 `t5` scene6 `t6` scene7 `t7` scene8 `t8` scene9 `t9` scene10 `t10` scene11 `t11` end
  where
    diffusionRate = 0.004
    baseParams = GS
        { feedRateU = const 0.029
        , killRateV = const 0.057
        , diffusionRateU = const (2 * diffusionRate * spatialResolution^2)
        , diffusionRateV = const (diffusionRate * spatialResolution^2)
        , step = 10 / temporalResolution
        }
    noise (Vec2 x y) z = fromMaybe 0 $ getValue perlin { perlinFrequency = 0.5 } (x / spatialResolution, y / spatialResolution, z)
    scene1 = baseParams
    t1 = transition 980 50 t
    scene2 = baseParams { step = 2 / temporalResolution }
    t2 = transition 1000 10 t
    scene3 = baseParams { killRateV = const 0.060, step = 2 / temporalResolution }
    t3 = transition 1050 100 t
    scene4 = baseParams { killRateV = const 0.062, step = 2 / temporalResolution }
    t4 = transition 1100 50 t
    scene5 = baseParams { killRateV = const 0.062, step = 5 / temporalResolution }
    t5 = transition 1600 50 t
    scene6 = baseParams { killRateV = const 0.062, feedRateU = const 0.045 }
    t6 = linearTransition 1600 1700 t
    scene7 = baseParams { killRateV = const 0.060, feedRateU = \p -> 0.040 + 0.015 * noise p (t/30) }
    t7 = linearTransition 1700 2200 t

    
    scene7 = baseParams
    t7 = transition 2200 50 t
    scene8 = baseParams { killRateV = const 0.065 }
    t8 = transition 2300 50 t
    scene9 = baseParams { killRateV = const 0.062, feedRateU = const 0.045 }
    t9 = linearTransition 2400 2500 t
    scene10 = baseParams { killRateV = const 0.060, feedRateU = \p -> 0.040 + 0.015 * noise p (t/30) }
    t10 = linearTransition 2500 3000 t
    scene11 = baseParams { killRateV = const 0.060, feedRateU = \p -> 0.030 + 0.015 * noise p (t/30) }
    t11 = transition 3000 50 t
    end = baseParams { killRateV = const 0.62 }
    t12

transition :: Double -> Double -> Double -> GrayScott -> GrayScott -> GrayScott
transition t0 duration t a b
    | t-t0 < (-duration/2) = a
    | t-t0 < duration/2    = interpolate (sigmoid (t0-t)) a b
    | otherwise            = b
  where sigmoid t' = 0.5 * (1 + tanh (pi*t'/duration))

linearTransition :: Double -> Double -> Double -> GrayScott -> GrayScott -> GrayScott
linearTransition t0 t1 t a b
    | t < t0 = a
    | t < t1 = interpolate ((t1-t) / deltaT) a b
    | otherwise = b
  where deltaT = t1 - t0

interpolate :: Double -> GrayScott -> GrayScott -> GrayScott
interpolate x a b = GS
    { feedRateU      = x *. feedRateU a      +. (1-x) *. feedRateU b
    , killRateV      = x *. killRateV a      +. (1-x) *. killRateV b
    , diffusionRateU = x *. diffusionRateU a +. (1-x) *. diffusionRateU b
    , diffusionRateV = x *. diffusionRateV a +. (1-x) *. diffusionRateV b
    , step           = x *. step a           +. (1-x) *. step b
    }

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
    x | x < 0.5   -> interpolateColor (2*x) color1 color0
      | otherwise -> interpolateColor (2*(x-0.5)) color2 color1
  where
    color0, color1, color2 :: (Double, Double, Double)
    color0 = (0, 0, 0.1)
    color1 = (0.1, 0.25, 0.5)
    color2 = (0.1, 0.9, 0.1)
    interpolateColor a c1 c2 = a *. c1 +. (1-a) *. c2

clamp :: (Ord a, Num a) => a -> a -> a -> a
clamp lower upper = max lower . min upper

type Grid = Plane (Double, Double, Double, Double)

data GrayScott = GS
    { feedRateU :: Vec2 -> Double
    , killRateV :: Vec2 -> Double
    , diffusionRateU :: Vec2 -> Double
    , diffusionRateV :: Vec2 -> Double
    , step :: Double
    }

grayScott :: Int -> GrayScott -> Grid -> Grid
grayScott steps GS{..} = repeatF steps (mapNeighbours grayScottStep)
  where
    grayScottStep x y (uv11, uv12, uv13, uv21, uv22, uv23, uv31, uv32, uv33) = (u0, v0, deltaU, deltaV) +. step *. (deltaU, deltaV, 0, 0)
      where
        p = Vec2 (fromIntegral x) (fromIntegral y)
        (u0, v0, _, _) = uv22
        deltaU = diffusionRateU p * laplaceU - u0 * v0^2 + feedRateU p * (1 - u0)
        deltaV = diffusionRateV p * laplaceV + u0 * v0^2 - (feedRateU p + killRateV p) * v0
        (laplaceU, laplaceV, _, _) = (uv11 +. 2*.uv12 +. uv13 +. 2*.uv21 -. 12*.uv22 +. 2*. uv23 +. uv31 +. 2*.uv32 +. uv33) /. 4

    repeatF :: Int -> (a -> a) -> a -> a
    repeatF 0 _ = id
    repeatF n f = f . repeatF (n-1) f
