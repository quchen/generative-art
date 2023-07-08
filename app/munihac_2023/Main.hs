{-# LANGUAGE TupleSections #-}
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
    polys <- shatter gen 10 (fmap (, zero) (transform (scale 600) haskellLogo))
    setColor black
    for_ polys $ \(poly, velocity) -> do
        let explosion = translate (t *. velocity)
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
        shards = cutPolygon scissors poly
        velocity2 = direction (perpendicularBisector scissors)
        addVelocity shard =
            let velocity2Signed = signum (cross (b -. a) (polygonCentroid shard -. a)) *. velocity2
            in (shard, velocity +. velocity2Signed)
        shardsWithNewVelocity = fmap addVelocity shards
    pure shardsWithNewVelocity

shatter :: GenIO -> Int -> [(Polygon, Vec2)] -> Cairo.Render [(Polygon, Vec2)]
shatter gen 0 polys = pure polys
shatter gen n polys = do
    shards <- for polys $ \poly -> do
        if polygonArea (fst poly) < 1000
            then pure [poly]
            else randomCut gen poly
    shatter gen (n-1) (concat shards)

randomColor :: GenIO -> Cairo.Render (Color Double)
randomColor gen = do 
    n <- Cairo.liftIO $ uniformRM (0, 1) gen
    pure (haskell n)

randomAngle :: GenIO -> Cairo.Render Angle
randomAngle gen = do
    angle <- Cairo.liftIO (uniformRM (0, 360) gen)
    pure (deg angle)
