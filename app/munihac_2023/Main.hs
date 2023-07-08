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

-- | You can generate the output files using either:
--
-- `stack build munihac2023 --file-watch --exec munihac-2023`
-- (slower compile time, faster execution time, works out of the box with `stack`)
--
-- `ghcid --command='stack ghci --main-is munihac2023' --test=main -W`
-- (fast compilation, slower execution time, requires `ghcid` installation)
main :: IO ()
main = for_ [0..1000 :: Int] $ \t -> do
    let file = printf "munihac2023/shatter%04d.png" t 
    putStrLn file
    render file 800 600 $ do
        setColor white
        Cairo.paint
        drawing (fromIntegral t / 10)

drawing :: Double -> Cairo.Render ()
drawing t = do
    gen <- Cairo.liftIO (initialize (V.fromList [4]))
    let rect = growPolygon (-100) $ Polygon [Vec2 0 0, Vec2 800 0, Vec2 800 600, Vec2 0 600]
        center = Vec2 400 300
    polys <- shatter gen 10 [(rect, zero)]
    setColor black
    for_ polys $ \(poly, velocity) -> do
        angle <- randomAngle gen
        let rotation = rotateAround (polygonCentroid poly) (angle /. 18)
            explosion = translate (t *. velocity)
        sketch (transform explosion growPolygon (-2) poly)
        color <- randomColor gen
        setColor black
        Cairo.strokePreserve
        setColor color
        Cairo.fill


-- fmap (+1) $ fmap (*2) $ [1, 2, 3]


randomCut :: GenIO -> (Polygon, Vec2) -> Cairo.Render [(Polygon, Vec2)]
randomCut gen (poly, velocity) = do
    let pivot = polygonCentroid poly
    angle <- randomAngle gen
    let scissors@(Line a b) = angledLine pivot angle 1
        [shard1, shard2] = cutPolygon scissors poly
        velocity2 = direction (perpendicularBisector scissors)
        velocity2Signed = signum (cross (b -. a) (polygonCentroid shard1 -. a)) *. velocity2
    pure [(shard1, velocity +. velocity2Signed), (shard2, velocity -. velocity2Signed)]

shatter :: GenIO -> Int -> [(Polygon, Vec2)] -> Cairo.Render [(Polygon, Vec2)]
shatter gen 0 polys = pure polys
shatter gen n polys = do
    shards <- for polys $ \poly -> do
        randomCut gen poly
    shatter gen (n-1) (concat shards)

randomColor :: GenIO -> Cairo.Render (Color Double)
randomColor gen = do 
    n <- Cairo.liftIO $ uniformRM (1, 200) gen
    pure (mma n)

randomAngle :: GenIO -> Cairo.Render Angle
randomAngle gen = do
    angle <- Cairo.liftIO (uniformRM (0, 360) gen)
    pure (deg angle)
