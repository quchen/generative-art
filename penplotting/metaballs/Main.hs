module Main (main) where


import Control.Monad
import Data.List
import qualified Data.Set as S
import System.Random.MWC

import Draw
import Draw.Plotting
import Geometry
import Geometry.Chaotic



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 400

canvas :: Polygon
canvas = Polygon [Vec2 0 0, Vec2 picWidth 0, Vec2 picWidth picHeight, Vec2 0 picHeight]

main :: IO ()
main = for_ [0..100 :: Int] $ \i -> do
    print i
    let count = 80
    gen <- initializeMwc i
    initialCenters <- replicateM count (uniformRM (Vec3 100 100 (-picHeight/2+50), Vec3 (picWidth-100) (picWidth-100) (picHeight/2-50)) gen)
    radii <- replicateM count (uniformRM (20, 40) gen)

    let metaballs = vsum $ zipWith ball radii initialCenters
        slice z (Vec2 x y) = metaballs (Vec3 x y z)
        layers =
            [ Polygon . toList . simplifyTrajectoryRdp 1 <$> transform (isometricPerspective z) outlines
            | z <- [-picHeight/2-60, -picHeight/2-55 .. picHeight/2+60]
            , let outlines = isoLines Grid { _range = (Vec2 (-60) (-60), Vec2 (picWidth+60) (picWidth+60)), _maxIndex = (picWidth `div` 5, picWidth `div` 5) } (slice z) 1
            ]
        clippedLayers = zipWith clipWithAbove layers (drop 1 (tails layers))
        penHoveringSettings = MinimizePenHoveringSettings { _getStartEndPoint = \(Polygon (p:_)) -> (p, p), _flipObject = Nothing, _mergeObjects = Nothing }
        fitToCanvas = transformBoundingBox clippedLayers canvas def
        paths = minimizePenHoveringBy penHoveringSettings $ S.fromList $ transform fitToCanvas $ concat clippedLayers

    let settings = def
            { _feedrate = 3000
            , _zTravelHeight = 5
            , _zDrawingHeight = -2
            --, _canvasBoundingBox = Just $ boundingBox canvas
            }
        plotResult = runPlot settings $ do
            plot canvas
            for_ paths plot

    --writeGCodeFile "out/metaballs.g" plotResult
    renderPreview ("out/metaballs" ++ show i ++ ".png") plotResult

ball :: Double -> Vec3 -> Vec3 -> Double
ball radius center q = (radius^2 / normSquare (center -. q))**1.7

isometricPerspective :: Double -> Transformation
isometricPerspective z
    =  translate (Vec2 0 (z / 1.4 - 100))
    <> scaleAround' origin 1 0.35
    <> rotateAround origin (deg (-45))
  where origin = Vec2 (picWidth/2) (picWidth/2)

clipWithAbove :: [Polygon] -> [[Polygon]] -> [Polygon]
clipWithAbove layer above = go layer (concat above)
  where
    go [] _ = []
    go xs [] = xs
    go xs (y:ys) = go (xs >>= \x -> fst <$> (x `differencePP` y)) ys
