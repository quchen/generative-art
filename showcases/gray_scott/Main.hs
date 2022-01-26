{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Map.Strict as M
import System.Random.MWC (create)
import Text.Printf (printf)
import qualified Codec.Picture as P

import Delaunay.Internal
import Draw
import Geometry hiding (Grid)
import Sampling
import qualified Codec.Picture.Types as P
import Control.Monad (when)

picWidth, picHeight :: Num a => a
picWidth = 200
picHeight = 200

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
        frames = take 500 (iterate (grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess . grayScottProcess) initialState)
    for_ (zip [0 :: Int ..] frames) $ \(index, grid) -> do
        let file = printf "out/gray_scott_%06i.png" index
        P.writePng file =<< renderImage grid

renderImage :: Grid -> IO (P.Image P.Pixel8)
renderImage grid = do
    img <- P.createMutableImage picWidth picHeight 255
    for_ (M.toList grid) $ \(Vec2 x y, ((u, _), _)) ->
        when (x >= 0 && x <= picWidth - 1 && y >= 0 && y <= picHeight - 1) $
            P.writePixel img (round x) (round y) (round (255 * u))
    P.freezeImage img

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
        deltaU = diffusionRateU * laplaceU - u0 * v0^2 + feedRateU * (1 - u0)
        deltaV = diffusionRateV * laplaceV + u0 * v0^2 - (feedRateU + killRateV) * v0
        (laplaceU, laplaceV) =
            let (u'', v'') = foldl' foo (0, 0) neighbours
                d = fromIntegral (length neighbours)
            in  (u'' / d, v'' / d)
        foo (u'', v'') q =
            let d = normSquare (q -. p)
                (u, v) = fst (grid M.! q)
            in (u'' + (u - u0) / d, v'' + (v - v0) / d)
