module Main (main) where


import Control.Monad
import Data.List
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC

import Draw
import Geometry
import Geometry.Algorithms.Clipping



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 400

main :: IO ()
main = do
    let count = 100
    gen <- create
    initialCenters <- replicateM count (uniformRM (Vec3 0 0 (-picHeight/2), Vec3 picWidth picHeight (picHeight/2)) gen)
    radii <- replicateM count (uniformRM (20, 60) gen)

    let metaballs = vsum $ zipWith ball radii initialCenters
        slice z (Vec2 x y) = metaballs (Vec3 x y z)
        layers =
            [ Polygon <$> transform (isometricPerspective z) outlines
            | z <- [-picHeight/2-60, -picHeight/2-55 .. picHeight/2+60]
            , let outlines = isoLines Grid { _range = (Vec2 (-60) (-60), Vec2 (picWidth+60) (picHeight+60)), _maxIndex = (picWidth `div` 5, picHeight `div` 5) } (slice z) 1
            ]
        clippedLayers = zipWith clipWithAbove layers (drop 1 (tails layers))

    render "out/metaballs.png" picWidth picHeight $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp picHeight)
        cairoScope (setColor white >> C.paint)
        for_ clippedLayers $ \outlines -> for_ outlines $ \outline -> do
            sketch outline
            setColor black >> C.stroke

ball :: Double -> Vec3 -> Vec3 -> Double
ball radius center q = (radius^2 / normSquare (center -. q))**1.7

isometricPerspective :: Double -> Transformation
isometricPerspective z
    =  translate (Vec2 0 (z / 1.4))
    <> scaleAround' origin 1 0.35
    <> rotateAround origin (deg 45)
  where origin = Vec2 (picWidth/2) (picHeight/2)

clipWithAbove :: [Polygon] -> [[Polygon]] -> [Polygon]
clipWithAbove layer above = go layer (concat above)
  where
    go [] _ = []
    go xs [] = xs
    go xs (y:ys) = go (xs >>= \x -> fst <$> (x `differencePP` y)) ys
