module Main (main) where

import Control.Monad
import Data.Foldable
import Data.Traversable
import qualified Data.Vector as V
import qualified Graphics.Rendering.Cairo as Cairo
import System.Random.MWC

import Geometry
import Draw
import Draw.Plotting
import Geometry.Coordinates.Hexagonal (polygonSketch)

-- | You can generate the output files using either:
--
-- `stack build munihac2022 --file-watch --exec munihac-2022-workshop`
-- (slower compile time, faster execution time, works out of the box with `stack`)
--
-- `ghcid --command='stack ghci --main-is munihac2022' --test=main -W`
-- (fast compilation, slower execution time, requires `ghcid` installation)
main :: IO ()
main = do
    gen <- initialize (V.fromList [4])

    let rect = Polygon
            [ Vec2 10 10
            , Vec2 10 200
            , Vec2 140 200
            , Vec2 140 10
            ]

    pieces <- fmap explode <$> shatter gen 2 [rect]

    let effects =
            [ \piece -> hatch piece (deg 30) 1
            , \piece -> hatch piece (deg 60) 2
            , \piece -> hatch piece (deg 90) 0.5
            ] ++ repeat (const [])

    render "munihac2022/shatter.png" 150 210 $ do
        setColor white
        Cairo.paint

        for_ pieces $ \piece -> do
            sketch piece
            setColor (black `withOpacity` 0.5)
            Cairo.fillPreserve
            setColor black
            Cairo.stroke

    let plotResult = runPlot def $ do
            for_ pieces $ \piece -> plot piece
            for_ (zipWith ($) effects pieces) $ \effect -> do
                pause PauseUserConfirm
                plot effect

    renderPreview "munihac2022/shatter_plot.png" plotResult
    writeGCodeFile "munihac2022/shatter_plot.g" plotResult

randomCut :: GenIO -> Polygon -> IO [Polygon]
randomCut gen poly = do
    let pivot = polygonCentroid poly
    angle <- uniformM gen
    let scissors = angledLine pivot angle 1
    pure (cutPolygon scissors poly)

shatter :: GenIO -> Int -> [Polygon] -> IO [Polygon]
shatter _ 0 polys = pure polys
shatter gen n polys = do
    shards <- for polys $ \poly -> randomCut gen poly
    shatter gen (n-1) (concat shards)

explode :: Polygon -> Polygon
explode poly = transform (translate displacement) poly
  where
    center = Vec2 150 210 /. 2
    displacement = 0.1 *. (polygonCentroid poly -. center)
