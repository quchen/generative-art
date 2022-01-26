{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map.Strict as M
import System.Random.MWC (create)
import Text.Printf (printf)
import qualified Codec.Picture as P

import Delaunay.Internal
import Draw
import Geometry
import Sampling
import qualified Codec.Picture.Types as P
import Control.Monad (when)

picWidth, picHeight :: Num a => a
picWidth = 100
picHeight = 100

latticeConstant :: Double
latticeConstant = 0.6

main :: IO ()
main = do
    gen <- create
    let width = picWidth
        height = picHeight
        k = 4

    seeds <- poissonDisc PoissonDisc { radius = 50, .. }
    vertices <- poissonDisc PoissonDisc { radius = latticeConstant, .. }
    let lattice = bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) vertices
        generator p = sum ((\q -> exp (- 0.05 * normSquare (p -. q))) <$> seeds)
        initialState = M.mapWithKey (\p ps -> ((1 - generator p, generator p), toList ps)) (vertexGraph lattice)
        grayScottProcess = grayScott GS
            { feedRateU = 0.029
            , killRateV = 0.057
            , diffusionRateU = 0.2
            , diffusionRateV = 0.1
            , width = picWidth
            , height = picHeight }
        frames = take 500 (iterate (grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess) initialState)
    for_ (zip [0 :: Int ..] frames) $ \(index, grid) -> do
        let file = printf "out/gray_scott_%06i.png" index
        P.writePng file =<< renderImage grid

renderImage :: Grid -> IO (P.Image P.Pixel8)
renderImage grid = do
    img <- P.createMutableImage picWidth picHeight 255
    for_ (M.toList grid) $ \(p, ((_, v), _)) ->
        for_ (antialias p v) $ \(x, y, v) ->
            when (x >= 0 && x <= picWidth - 1 && y >= 0 && y <= picHeight - 1) $ do
                px0 <- P.readPixel img x y
                let v0 = fromIntegral px0 / 255 :: Double
                    clamp = max 0 . min 1
                    v' = clamp (v0 - v)
                P.writePixel img x y (round (255 * v'))
    P.freezeImage img

antialias :: Vec2 -> Double -> [(Int, Int, Double)]
antialias (Vec2 x y) px =
    [ (x', y', px * exp (- 0.5 * ((fromIntegral x' - x)^2 + (fromIntegral y' - y)^2) / latticeConstant ^ 2))
    | x' <- let x0 = round x in [x0 - 2 .. x0 + 2]
    , y' <- let y0 = round y in [y0 - 2 .. y0 + 2]
    ]


type Grid = M.Map Vec2 ((Double, Double), [Vec2])

data GrayScott = GS
    { feedRateU :: Double
    , killRateV :: Double
    , diffusionRateU :: Double
    , diffusionRateV :: Double
    , width :: Double
    , height :: Double
    }

grayScott :: GrayScott -> Grid -> Grid
grayScott GS{..} grid = M.mapWithKey grayScottStep grid
  where
    grayScottStep p (uv0@(u0, v0), neighbours) = ((u0 + deltaU, v0 + deltaV), neighbours)
      where
        deltaU = diffusionRateU * laplaceU - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplaceV + u0 * v0^2 - (feedRateU + killRateV) * v0
        (laplaceU, laplaceV) = foldl' (\uv q -> uv +. delta q) (0, 0) neighbours /. fromIntegral (length neighbours)
        delta q = (fst (grid M.! q) -. uv0) /. norm (q -. p)
