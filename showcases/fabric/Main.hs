module Main (main) where



import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Math.Noise               (Perlin (..), getValue, perlin)
import           Prelude                  hiding ((**))
import           System.Random.MWC

import Draw
import Geometry                     as G



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

scaleFactor :: Double
scaleFactor = 1

main :: IO ()
main = pure ()
