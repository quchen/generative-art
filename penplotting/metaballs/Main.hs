module Main (main) where


import Data.Traversable
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC

import Geometry
import Geometry.Algorithms.Sampling
import Draw


picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 400

canvas :: BoundingBox
canvas = boundingBox [zero, Vec2 picWidth picHeight]

main :: IO ()
main = do
    gen <- create
    centers <- poissonDisc gen def { _poissonShape = canvas, _poissonRadius = 100, _poissonK = 5 }
    metaballs <- fmap vsum $ for centers $ \center -> do
        radius <- uniformRM (20, 60) gen
        pure (ball radius center)
    let outlines = isoLines Grid { _range = (zero, Vec2 picWidth picHeight), _maxIndex = (picWidth `div` 5, picHeight `div` 5) } metaballs 1

    render "out/metaballs.png" picWidth picHeight $ do
        cairoScope (setColor white >> C.paint)
        for_ outlines $ sketch . Polyline
        C.stroke
    pure ()

ball :: Double -> Vec2 -> Vec2 -> Double
ball radius center q = (radius^2 / normSquare (center -. q))^2
