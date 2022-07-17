module Main (main) where


import Control.Monad
import Data.List
import qualified Data.Set as S
import qualified Graphics.Rendering.Cairo as C
import System.Random.MWC

import Draw
import Draw.Plotting
import Geometry



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
            [ Polygon . toList . simplifyTrajectoryRdp 1 <$> transform (isometricPerspective z) outlines
            | z <- [-picHeight/2-60, -picHeight/2-55 .. picHeight/2+60]
            , let outlines = isoLines Grid { _range = (Vec2 (-60) (-60), Vec2 (picWidth+60) (picHeight+60)), _maxIndex = (picWidth `div` 5, picHeight `div` 5) } (slice z) 1
            ]
        clippedLayers = zipWith clipWithAbove layers (drop 1 (tails layers))
        penHoveringSettings = MinimizePenHoveringSettings { _getStartEndPoint = \(Polygon (p:ps)) -> (p, p), _flipObject = Nothing, _mergeObjects = Nothing }
        paths = minimizePenHoveringBy penHoveringSettings $ S.fromList $ concat clippedLayers

    let settings = def
            { _feedrate = 3000
            , _zTravelHeight = 5
            , _zDrawingHeight = -2
            }
        plotResult = runPlot settings $ for_ paths plot

    writeGCodeFile "out/metaballs.g" plotResult
    renderPreview "out/metaballs.png" plotResult

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
