module Main (main) where



import Data.Foldable
import Graphics.Rendering.Cairo as C

import Draw
import Geometry
import Geometry.Shapes
import Geometry.Coordinates.Hexagonal as Hex



picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 1000

main :: IO ()
main = withSurfaceAuto "out/hexagonal.svg" picWidth picHeight renderDrawing
  where
    renderDrawing surface = renderWith surface drawing

drawing :: Render ()
drawing = do
    C.translate 500 500
    C.scale 1.5 1.5
    hexagonalCoordinateSystem 50 2
