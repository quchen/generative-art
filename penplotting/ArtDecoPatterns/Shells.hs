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
            { _previewPenWidth = 0.5
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
gcodeDrawing = for_ [(2 * x + y `mod` 2, y) | x <- [0..6], y <- [0..9]] $ \(x, y) -> do
    let p1 = fromIntegral x *. gridX +. fromIntegral y *. gridY -. gridY
        p2 = p1 +. gridX +. gridY
        p3 = p1 +. 2 *. gridY
        p4 = p1 -. gridX +. gridY
    for_ [1/9, 3/9 .. 0.8] $ \l -> do
        let p2' = (1 - l) *. p2 +. l *. p3
            p4' = (1 - l) *. p4 +. l *. p3
            p2'' = (1 - l - 0.1) *. p2 +. (l + 0.1) *. p3
            p4'' = (1 - l - 0.1) *. p4 +. (l + 0.1) *. p3
            poly1 = Polygon [p1, p2', p2'']
            poly2 = Polygon [p1, p4', p4'']
        plot (zigzag (hatch poly1 (angleOfLine (Line p1 p2')) 0.5))
        plot [Line p1 p2'', Line p2'' p2']
        plot (zigzag (hatch poly2 (angleOfLine (Line p1 p4')) 0.5))
        plot [Line p1 p4'', Line p4'' p4']
  where
    radius = 50
    gridX = Vec2 radius 0
    gridY = Vec2 0 radius
    zigzag = Polyline . go
      where
        go [] = []
        go [Line a b] = [a, b]
        go (Line a b : Line c d : ls) = a : b : d : c : go ls
