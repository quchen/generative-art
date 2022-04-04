module Main (main) where

import Draw.Plotting
import Geometry
import Geometry.Algorithms.Clipping
import qualified Geometry.Shapes as Shapes (haskellLogo)

-- DIN A4
picWidth, picHeight :: Double
picWidth = 297
picHeight = 210

haskellLogo :: [Polygon]
haskellLogo = transform (scale 297 <> mirrorAlong (angledLine (Vec2 0 0.5) (deg 0) 1)) Shapes.haskellLogo

main :: IO ()
main = pure ()
