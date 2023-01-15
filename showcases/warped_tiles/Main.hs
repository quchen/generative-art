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
import Geometry.Algorithms.Sampling
import Control.Monad (replicateM)
import Control.Applicative (Applicative(liftA2))
import Debug.Trace
import Numerics.VectorAnalysis (grad)
import Numerics.DifferentialEquation (rungeKuttaAdaptiveStep)



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
    for_ [-1, -0.9 .. 2] $ \z -> do
        for_ (isoLines GridSpec { _range = (zero, Vec2 picWidth picHeight), _maxIndex = (128, 72)} potential z) $ \isoline -> do
            sketch (Polyline isoline)
            C.stroke
    for_ [Vec2 1750 750, Vec2 1000 900] $ \p -> do
        sketch (fieldLine p)
        C.stroke
    for_ [ p +. polar phi 50 | (p, _) <- charges, phi <- rad <$> [0, 0.05*pi .. 2*pi] ] $ \p -> do
        sketch (fieldLine p)
        C.stroke

charges :: [(Vec2, Double)]
charges = [ (Vec2 500 300, 1000), (Vec2 2000 500, -1000), (Vec2 1500 1000, 1000) ]

potential :: Vec2 -> Double
potential p = sum [ q / (picWidth / 5 + norm (p -. p')) | (p', q) <- charges ]
  where

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

