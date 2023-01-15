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



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

file :: FilePath
file = "out/warped-tiles.png"

main :: IO ()
main = render file picWidth picHeight $ do
    cairoScope (setColor white >> C.paint)
    setColor black
    C.setLineWidth 10
    for_ [-1, -0.9 .. 2] $ \z -> do
        for_ (isoLines GridSpec { _range = (zero, Vec2 picWidth picHeight), _maxIndex = (128, 72)} potential z) $ \isoline -> do
            sketch (Polyline $ traceShowId isoline)
            C.stroke

potential :: Vec2 -> Double
potential p = sum [ q / (picWidth / 5 + norm (p -. p')) | (p', q) <- charges ]
  where
    charges = [ (Vec2 500 300, 1000), (Vec2 2000 500, -1000), (Vec2 1500 1000, 1000) ]
