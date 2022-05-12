module Main (main) where


import Control.Monad
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC
import Text.Printf

import Draw
import Geometry
import Geometry.Algorithms.Sampling
import Numerics.DifferentialEquation
import Physics



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 400

canvas :: BoundingBox
canvas = boundingBox [zero, Vec2 picWidth picHeight]

main :: IO ()
main = do
    gen <- create
    initialCenters <- poissonDisc gen def { _poissonShape = canvas, _poissonRadius = 100, _poissonK = 5 }
    radii <- replicateM (length initialCenters) (uniformRM (20, 60) gen)

    let particles = NBody (PhaseSpace zero <$> initialCenters)
        pressure = 3
        externalPotential = harmonicPotential (picWidth / pressure, picHeight / pressure) (Vec2 (picWidth/2) (picHeight/2))
        interactionPotential = coulombPotential (picWidth / 6)
        frames = fmap (fmap q . getNBody . snd) $ take 1000 $ rungeKuttaConstantStep (const (nBody externalPotential interactionPotential (pure 1))) particles 0 10

    for_ (zip [0 :: Int ..] frames) $ \(i, centers) -> do
        let metaballs = vsum $ zipWith ball radii centers
            outlines = isoLines Grid { _range = (zero, Vec2 picWidth picHeight), _maxIndex = (picWidth `div` 5, picHeight `div` 5) } metaballs 1
        render (printf "out/metaballs_%03d.png" i) picWidth picHeight $ do
            cairoScope (setColor white >> C.paint)
            for_ outlines $ sketch . Polyline
            C.stroke

ball :: Double -> Vec2 -> Vec2 -> Double
ball radius center q = (radius^2 / normSquare (center -. q))^2
