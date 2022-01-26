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

picWidth, picHeight :: Num a => a
picWidth = 100
picHeight = 100

main :: IO ()
main = do
    gen <- create
    let width = picWidth
        height = picHeight
        k = 4

    seeds <- poissonDisc PoissonDisc { radius = 50, .. }
    vertices <- poissonDisc PoissonDisc { radius = 1, .. }
    let lattice = bowyerWatson (BoundingBox (Vec2 0 0) (Vec2 picWidth picHeight)) vertices
        generator p = sum ((\q -> exp (- 0.5 * normSquare (p -. q))) <$> seeds)
        initialState = M.mapWithKey (\p ps -> ((1 - generator p, generator p), toList ps)) (vertexGraph lattice)
        grayScottProcess = grayScott GS
            { feedRateU = 0.029
            , killRateV = 0.057
            , diffusionRateU = 0.2
            , diffusionRateV = 0.1
            , width = picWidth
            , height = picHeight }
        frames = take 5 (iterate (grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess) initialState)
    for_ (zip [0 :: Int ..] frames) $ \(index, grid) -> do
        let file = printf "out/gray_scott_%06i.png" index
        P.writePng file (renderImage grid)

renderImage :: Grid -> P.Image P.Pixel8
renderImage grid = P.generateImage pixel picWidth picHeight
  where

    pixel x y = round $ 255 * (sum pointsWithinRadius / fromIntegral (length pointsWithinRadius))
      where
        p = Vec2 (fromIntegral x) (fromIntegral y)
        pointsWithinRadius = fst . fst . snd <$> M.toList (M.filterWithKey (\q _ -> norm (q -. p) < 2) grid)

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
    grayScottStep p ((u0, v0), neighbours) = ((u0 + deltaU, v0 + deltaV), neighbours)
      where
        u = fst . fst . (grid M.!)
        v = snd . fst . (grid M.!)
        deltaU = diffusionRateU * laplace u - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplace v + u0 * v0^2 - (feedRateU + killRateV) * v0
        laplace f = sum (delta f <$> neighbours) / fromIntegral (length neighbours)
        delta f q = (f q - f p) / normSquare (q -. p)
