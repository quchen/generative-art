module Main (main) where



import Data.Foldable
import Graphics.Rendering.Cairo as C hiding (x,y)

import Draw
import Geometry as G
import Geometry.Shapes
import Geometry.Coordinates.Hexagonal as Hex



picWidth, picHeight :: Num a => a
picWidth = 1000
picHeight = 1000

main :: IO ()
main = withSurfaceAuto "out/hexagonal.svg" picWidth picHeight renderDrawing
  where
    renderDrawing surface = renderWith surface drawing

hexagonSketch :: Double -> Vec2 -> Render ()
hexagonSketch sideLength center = polygonSketch (G.transform (G.translate center <> G.scale sideLength <> G.rotate (deg 30)) regularPolygon 6)

drawing :: Render ()
drawing = do
    let sideLength = 50
    C.translate 500 500
    C.scale 1.5 1.5
    hexagonalCoordinateSystem sideLength 3

    for_ (Hex.line (Cube (-1) 0 1) (Cube 3 (-2) (-1))) $ \hexLineSegment -> do
        cairoScope $ do
            hexagonSketch sideLength (toVec2 sideLength hexLineSegment)
            mmaColor 0 0.3
            fillPreserve
            mmaColor 0 0.5
            stroke
