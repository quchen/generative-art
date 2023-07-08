{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC

import Geometry
import Draw
import Geometry.Chaotic (initializeMwc)
import Data.Traversable
import Text.Printf (printf)
import Geometry.Shapes (haskellLogo)

-- | You can generate the output files using either:
--
-- `stack build munihac2023 --file-watch --exec munihac-2023`
-- (slower compile time, faster execution time, works out of the box with `stack`)
--
-- `ghcid --command='stack ghci --main-is munihac2023' --test=main -W`
-- (fast compilation, slower execution time, requires `ghcid` installation)
--
-- To render a video: ffmpeg -i munihac2023/shatter%04d.png -vf "setpts=N/60/TB,framerate=fps=60" -c:v libx264 -pix_fmt yuv420p munihac2023.mp4
main :: IO ()
main = for_ [0..1000 :: Int] $ \t -> do
    let file = printf "munihac2023/shatter%04d.png" t 
    putStrLn file
    render file 800 600 $ do
        setColor white
        Cairo.paint
        drawing (fromIntegral (1000 - t) / 10)

drawing :: Double -> Cairo.Render ()
drawing t = do
    gen <- Cairo.liftIO (initialize (V.fromList [4]))
    shards <- shatter gen 10 $ zipWith3 Shard
        (transform (scale 600) haskellLogo)
        (repeat zero)
        (fmap haskell [0, 0.5, 1, 1])
    setColor black
    for_ shards $ \Shard{..} -> do
        let explosion = translate (t *. velocity)
        sketch (transform explosion poly)
        setColor color
        Cairo.strokePreserve
        Cairo.fill


data Shard = Shard
    { poly :: Polygon
    , velocity :: Vec2
    , color :: Color Double
    }

randomCut :: GenIO -> Shard -> Cairo.Render [Shard]
randomCut gen Shard{..} = do
    let pivot = polygonCentroid poly
    angle <- randomAngle gen
    let scissors@(Line a b) = angledLine pivot angle 1
        shards = cutPolygon scissors poly
        velocity2 = direction (perpendicularBisector scissors)
        addVelocity shard =
            let velocity2Signed = signum (cross (b -. a) (polygonCentroid shard -. a)) *. velocity2
            in Shard { poly = shard, velocity = velocity +. velocity2Signed, color = color }
        shardsWithNewVelocity = fmap addVelocity shards
    pure shardsWithNewVelocity

shatter :: GenIO -> Int -> [Shard] -> Cairo.Render [Shard]
shatter gen 0 polys = pure polys
shatter gen n polys = do
    shards <- for polys $ \shard -> do
        if polygonArea (poly shard) < 1000
            then pure [shard]
            else randomCut gen shard
    shatter gen (n-1) (concat shards)

randomColor :: GenIO -> Cairo.Render (Color Double)
randomColor gen = do 
    n <- Cairo.liftIO $ uniformRM (0, 1) gen
    pure (haskell n)

randomAngle :: GenIO -> Cairo.Render Angle
randomAngle gen = do
    angle <- Cairo.liftIO (uniformRM (0, 360) gen)
    pure (deg angle)
