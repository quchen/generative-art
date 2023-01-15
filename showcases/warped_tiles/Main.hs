{-# LANGUAGE RecordWildCards #-}
module Main (main, main0, main1, main2, main3) where



import Data.List ( sortOn )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as C
import Math.Noise (Perlin (..), getValue, perlin)
import System.Random.MWC ( initialize, uniformRM )

import Draw
import Geometry as G
import Geometry.Algorithms.Delaunay
import Geometry.Algorithms.Sampling
import Geometry.Algorithms.Sampling.Vogel
import Geometry.Algorithms.Voronoi
import Control.Monad (replicateM)
import Control.Applicative (Applicative(liftA2))



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

file :: FilePath
file = "out/warped-tiles.png"

main :: IO ()
main = pure ()
