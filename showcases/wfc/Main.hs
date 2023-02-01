module Main (main) where



import           Data.List
import qualified Data.MultiSet as M
import           Data.Ord
import           Graphics.Rendering.Cairo as C hiding (height, width, x, y)
import           Prelude                  hiding ((**))
import           System.Random.MWC

import Data.Grid.Hexagonal as Grid
import Draw hiding (Cross)
import Draw.Grid
import Geometry                     as G
import Geometry.Coordinates.Hexagonal
import Geometry.Algorithms.WaveFunctionCollapse
import Control.Monad.ST (runST)



picWidth, picHeight :: Num a => a
picWidth = 2560
picHeight = 1440

cellSize :: Num a => a
cellSize = 40

main :: IO ()
main = do
    render "showcases/wfc/template.svg" 1440 1440 $ do
        coordinateSystem $ MathStandard_ZeroCenter_XRight_YUp 1440 1440
        cairoScope (setColor white >> paint)
        hexagonalCoordinateSystem 72 8
        pure ()
