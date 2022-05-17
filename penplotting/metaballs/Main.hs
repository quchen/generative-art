module Main (main) where


import Control.Monad
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC
import Text.Printf

import Draw
import Geometry
import Geometry.Core.ThreeD (Vec3(..))
import Numerics.DifferentialEquation
import Physics



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 400

main :: IO ()
main = do
    let count = 20
    gen <- create
    initialCenters <- replicateM count (uniformRM (Vec3 0 0 (-picHeight/2), Vec3 picWidth picHeight (picHeight/2)) gen)
    radii <- replicateM count (uniformRM (20, 60) gen)

    let particles = NBody (PhaseSpace zero <$> initialCenters)
        pressure = 5
        externalPotential = harmonicPotential3D (picWidth / pressure, picHeight / pressure, picHeight / pressure) (Vec3 (picWidth/2) (picHeight/2) 0)
        interactionPotential = coulombPotential (picWidth / 6)
        frames = fmap (fmap q . getNBody . snd) $ take 1200 $ rungeKuttaConstantStep (const (nBody externalPotential interactionPotential (pure 1))) particles 0 3

    for_ (zip [0 :: Int ..] frames) $ \(i, centers) -> do
        let metaballs (Vec2 x y) = vsum (zipWith ball radii centers) (Vec3 x y 0)
            outlines = isoLines Grid { _range = (zero, Vec2 picWidth picHeight), _maxIndex = (picWidth `div` 5, picHeight `div` 5) } metaballs 1
        render (printf "out/metaballs_%03d.png" i) picWidth picHeight $ do
            cairoScope (setColor white >> C.paint)
            for_ outlines $ sketch . Polyline
            C.stroke

ball :: Double -> Vec3 -> Vec3 -> Double
ball radius center q = (radius^2 / normSquare (center -. q))^2
