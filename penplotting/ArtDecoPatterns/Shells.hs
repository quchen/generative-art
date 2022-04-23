module Main (main) where



import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as C

import Geometry
import Draw
import Draw.Plotting



picWidth, picHeight :: Num a => a
picWidth = 600
picHeight = 450

main :: IO ()
main = do
    let settings = def
            { _zDrawingHeight = -2
            , _zTravelHeight = 5
            , _feedrate = 15000
            , _previewPenWidth = 0.5
            , _previewPenTravelColor = Nothing
            , _previewDecorate = False
            }
    let result = runPlot settings gcodeDrawing
    writeGCodeFile "shells-pattern.g" result
    render "out/shells-pattern.png" picWidth picHeight $ do
        cairoScope (setColor black >> C.paint)
        C.transform (C.Matrix 1 0 0 (-1) 0 picHeight)
        _plotPreview result
        pure ()

gcodeDrawing :: Plot ()
gcodeDrawing = for_ [ (x, y) | x <- [1..23], y <- [x `mod` 2 + 1, x `mod` 2 + 3 .. 17]] $ \(x, y) -> do
    let p1 = fromIntegral x *. gridX +. fromIntegral y *. gridY -. gridX
        p2 = p1 +. gridX -. gridY
        p3 = p1 +. 2 *. gridX
    for_ [1/9, 3/9 .. 0.8] $ \l -> do
        let p2' = (1 - l) *. p2 +. l *. p3
            p2'' = (1 - l - 0.1) *. p2 +. (l + 0.1) *. p3
            mirror = mirrorAlong (angledLine p1 (deg 0) 1)
            poly = Polygon [p1, p2', p2'']
            hatching = zigzag (hatch poly (angleOfLine (Line p1 p2')) 0.5)
        plot hatching
        plot (reversePolyline $ transform mirror hatching)
        plot (Polygon (vertices poly ++ vertices (transform mirror poly)))
  where
    radius = 25
    gridX = Vec2 radius 0
    gridY = Vec2 0 radius
    zigzag = Polyline . go
      where
        go [] = []
        go [Line a b] = [a, b]
        go (Line a b : Line c d : ls) = a : b : d : c : go ls
    vertices (Polygon ps) = ps
    reversePolyline (Polyline ps) = Polyline (reverse ps)
