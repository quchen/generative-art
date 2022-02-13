{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as A
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import qualified Data.Array.Accelerate.IO.Codec.Picture as A
import Text.Printf (printf)
import qualified Codec.Picture as P
import System.Environment (getArgs)

import Draw
import Geometry hiding (Grid)
import Plane
import AccVectorSpace

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
simulation t0 initialState = takeWhile ((< 50) . fst) (iterate (\(t, state) -> (t + 1, grayScott temporalResolution (scene (fromIntegral t)) state)) (t0, initialState))

writeOutput :: [(Int, Grid)] -> IO ()
writeOutput frames = for_ frames $ \(index, grid) -> do
    P.writePng (printf "out/gray_scott_%06i.png" index) (renderImageColor (\uv -> colorFront uv +.. colorTrail uv +.. colorReaction uv) grid)
    P.writePng (printf "out/uv_gray_scott_%06i.png" index) (renderImageColor (\(A.T4 u v _ _) -> A.lift (1-u-v, v, u)) grid)

scene :: Double -> GrayScott
scene t = scene1 `t1` scene2 `t2` scene3 `t3` scene4 `t4` scene5
  where
    diffusionRate = 0.004
    baseParams = GS
        { feedRateU = 0.029
        , killRateV = 0.057
        , diffusionRateU = 2 * diffusionRate * spatialResolution^2
        , diffusionRateV = diffusionRate * spatialResolution^2
        , step = 10 / temporalResolution
        }
    scene1 = baseParams
    t1 = transition 580 50 t
    scene2 = baseParams { step = 2 / temporalResolution }
    t2 = transition 600 10 t
    scene3 = baseParams { killRateV = 0.060, step = 2 / temporalResolution }
    t3 = transition 650 100 t
    scene4 = baseParams { killRateV = 0.062, step = 2 / temporalResolution }
    t4 = transition 700 50 t
    scene5 = baseParams { killRateV = 0.062, step = 5 / temporalResolution }

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

renderImageColor :: (A.Exp (Double, Double, Double, Double) -> A.Exp (Double, Double, Double)) -> Grid -> P.Image P.PixelRGB8
renderImageColor f plane = A.imageOfArray (CPU.run (A.map renderPixel (A.use plane)))
  where
    renderPixel uv = let A.T3 r g b = f uv in A.lift (A.PixelRGB8_ (pixel8 r) (pixel8 g) (pixel8 b))
    pixel8 = A.round . clamp 0 255 . (* 255)

runExp :: A.Elt e => A.Exp e -> e
runExp e = A.indexArray (A.run (A.unit e)) A.Z

colorFront :: A.Exp (Double, Double, Double, Double) -> A.Exp (Double, Double, Double)
colorFront (A.T4 _ _ _ dv) = A.tanh (400 * max 0 dv) *.. A.constant (0.4, 0.1, 0)

colorTrail :: A.Exp (Double, Double, Double, Double) -> A.Exp (Double, Double, Double)
colorTrail (A.T4 _ _ du dv) = A.tanh (400 * max 0 du) *.. A.constant (0, 0, 0.5) +.. A.tanh (-400 * min 0 dv) *.. A.constant (0.7, 0, -0.1)

colorReaction :: A.Exp (Double, Double, Double, Double) -> A.Exp (Double, Double, Double)
colorReaction (A.T4 u v _ _) =
    let x = A.tanh (30 * u * v * v)
    in  A.ifThenElse (x A.< 0.5)
            (interpolateColor (2*x) color1 color0)
            (interpolateColor (2*(x-0.5)) color2 color1)
  where
    color0, color1, color2 :: A.Exp (Double, Double, Double)
    color0 = A.constant (0, 0, 0.1)
    color1 = A.constant (0.1, 0.25, 0.5)
    color2 = A.constant (0.1, 0.9, 0.1)
    interpolateColor a c1 c2 = a *.. c1 +.. (1-a) *.. c2

clamp :: (A.Ord a, A.Num a) => A.Exp a -> A.Exp a -> A.Exp a -> A.Exp a
clamp lower upper = A.max lower . A.min upper

type Grid = A.Matrix (Double, Double, Double, Double)

data GrayScott = GS
    { feedRateU :: Double
    , killRateV :: Double
    , diffusionRateU :: Double
    , diffusionRateV :: Double
    , step :: Double
    }

type UV = (Double, Double, Double, Double)

grayScott :: Int -> GrayScott -> Grid -> Grid
grayScott steps GS{..} grid = CPU.run $ repeatF steps (mapNeighbours grayScottStep) (A.use grid)
  where
    grayScottStep :: A.Stencil3x3 UV -> A.Exp UV
    grayScottStep ((uv11, uv12, uv13), (uv21, uv22, uv23), (uv31, uv32, uv33)) = A.lift (u0 A.+ A.constant step A.* deltaU, v0 A.+ A.constant step A.* deltaV, deltaU, deltaV)
      where
        A.T4 u0  v0  _ _ = uv22
        A.T4 u11 v11 _ _ = uv11
        A.T4 u12 v12 _ _ = uv12
        A.T4 u13 v13 _ _ = uv13
        A.T4 u21 v21 _ _ = uv21
        A.T4 u22 v22 _ _ = uv22
        A.T4 u23 v23 _ _ = uv23
        A.T4 u31 v31 _ _ = uv31
        A.T4 u32 v32 _ _ = uv32
        A.T4 u33 v33 _ _ = uv33
        deltaU = A.constant diffusionRateU * laplaceU - u0 * v0^2 + A.constant feedRateU * (1 - u0)
        deltaV = A.constant diffusionRateV * laplaceV + u0 * v0^2 - A.constant (feedRateU + killRateV) * v0
        laplaceU = (u11 A.+ 2 A.* u12 A.+ u13 A.+ 2 A.* u21 A.- 12 A.* u22 A.+ 2 A.* u23 A.+ u31 A.+ 2 A.* u32 A.+ u33) A./ 4
        laplaceV = (v11 A.+ 2 A.* v12 A.+ v13 A.+ 2 A.* v21 A.- 12 A.* v22 A.+ 2 A.* v23 A.+ v31 A.+ 2 A.* v32 A.+ v33) A./ 4

    repeatF :: A.Arrays a => Int -> (A.Acc a -> A.Acc a) -> A.Acc a -> A.Acc a
    repeatF 0 _ acc = acc
    repeatF n f acc = f (repeatF (n A.- 1) f acc)
