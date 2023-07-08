module Main (main) where

import Control.Monad
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC

import Geometry
import Draw
import Geometry.Chaotic (initializeMwc)
import Data.Traversable

-- | You can generate the output files using either:
--
-- `stack build munihac2023 --file-watch --exec munihac-2023`
-- (slower compile time, faster execution time, works out of the box with `stack`)
--
-- `ghcid --command='stack ghci --main-is munihac2023' --test=main -W`
-- (fast compilation, slower execution time, requires `ghcid` installation)
main :: IO ()
main = do
    render "munihac2023/shatter.png" 800 600 $ do
        setColor white
        Cairo.paint
        drawing

drawing :: Cairo.Render ()
drawing = do
    gen <- Cairo.liftIO (initialize (V.fromList [4]))
    let rect = growPolygon (-100) $ Polygon [Vec2 0 0, Vec2 800 0, Vec2 800 600, Vec2 0 600]
        center = Vec2 400 300
    polys <- shatter gen 10 [rect]
    setColor black
    for_ polys $ \poly -> do
        angle <- randomAngle gen
        let rotation = rotateAround (polygonCentroid poly) (angle /. 18)
            distanceFromCenter = polygonCentroid poly -. center
            explosion = translate (0.002 *. norm distanceFromCenter *. distanceFromCenter)
        sketch (transform (explosion <> rotation) growPolygon (-2) poly)
        color <- randomColor gen
        setColor black
        Cairo.strokePreserve
        setColor color
        Cairo.fill


randomCut :: GenIO -> Polygon -> Cairo.Render [Polygon]
randomCut gen poly = do
    let pivot = polygonCentroid poly
    angle <- randomAngle gen
    let scissors = angledLine pivot angle 1
    pure (cutPolygon scissors poly)

shatter :: GenIO -> Int -> [Polygon] -> Cairo.Render [Polygon]
shatter gen 0 polys = pure polys
shatter gen n polys = do
    shards <- for polys $ \poly -> do
        angle <- randomAngle gen
        randomCut gen (transform (rotateAround (polygonCentroid poly) (angle /. 180)) poly)
    shatter gen (n-1) (concat shards)

randomColor :: GenIO -> Cairo.Render (Color Double)
randomColor gen = do 
    n <- Cairo.liftIO $ uniformRM (1, 200) gen
    pure (mma n)

randomAngle :: GenIO -> Cairo.Render Angle
randomAngle gen = do
    angle <- Cairo.liftIO $ uniformRM (0, 360) gen
    pure (deg angle)
