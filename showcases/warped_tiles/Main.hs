{-# LANGUAGE RecordWildCards #-}
module Main (main) where



import Data.List ( sortOn )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as C
import System.Random.MWC ( initialize, uniformRM )

import Draw
import Geometry as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Voronoi
import Geometry.Algorithms.Sampling
import Control.Monad (replicateM)
import Control.Applicative (Applicative(liftA2))
import Debug.Trace
import Numerics.VectorAnalysis (grad)
import Numerics.DifferentialEquation (rungeKuttaAdaptiveStep)
import Geometry.Algorithms.Voronoi (Voronoi(_voronoiCells))
import Data.List.Extended (nubOrd)



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

canvas :: BoundingBox
canvas = boundingBox [zero, Vec2 picWidth picHeight]

file :: FilePath
file = "out/warped-tiles.png"

main :: IO ()
main = render file picWidth picHeight $ do
    cairoScope (setColor white >> C.paint)
    setColor black
    C.setLineWidth 10
    let potentialLines = 
            [ Polyline isoline
            | z <- [-1, -0.9 .. 2]
            , isoline <- isoLines GridSpec { _range = (zero, Vec2 picWidth picHeight), _maxIndex = (128, 72)} potential z
            ]
        fieldLines =
            [ fieldLine (p +. polar phi 50)
            | (p, _) <- charges
            , phi <- rad <$> [0, 0.2*pi .. 2*pi]
            ]
        intersectionPoints = nubOrd $ do
            pl <- potentialLines
            fl <- fieldLines
            lineIntersections pl fl
        cells = _voronoiCells $ toVoronoi $ bowyerWatson canvas intersectionPoints
    for_ cells $ \VoronoiCell{..} -> do
        sketch (growPolygon (-5) _voronoiRegion)
        C.fill

charges :: [(Vec2, Double)]
charges = [ (Vec2 500 300, 1000), (Vec2 2000 500, -1000), (Vec2 1500 1000, 1000) ]

potential :: Vec2 -> Double
potential p = sum [ q / (picWidth / 5 + norm (p -. p')) | (p', q) <- charges ]

vectorField :: Vec2 -> Vec2
vectorField = grad potential

fieldLine :: Vec2 -> Polyline []
fieldLine p = Polyline (reverse trajectoryB ++ trajectoryA)
  where
    trajectory vf = rungeKuttaAdaptiveStep (const vf) p 0 0.1 normSquare 0.001
    trajectoryA = takeWhile (`insideBoundingBox` canvas) $ takeUntil hitsCharge (snd <$> trajectory vectorField)
    trajectoryB = takeWhile (`insideBoundingBox` canvas) $ takeUntil hitsCharge (snd <$> trajectory (negateV vectorField))

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = takeWhile (not . p)

hitsCharge :: Vec2 -> Bool
hitsCharge p = any (\(p', _) -> norm (p -. p') < 10) charges

lineIntersections :: Polyline [] -> Polyline [] -> [Vec2]
lineIntersections (Polyline xs) (Polyline ys) = 
    [ p
    | l1 <- zipWith Line xs (tail xs)
    , l2 <- zipWith Line ys (tail ys)
    , p <- case intersectionLL l1 l2 of
        IntersectionReal p -> [p]
        _otherwise -> []
    ]
