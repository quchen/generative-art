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
    writeGCodeFile "fish-scales-pattern.g" result
    render "out/fish-scales-pattern.png" picWidth picHeight $ do
        cairoScope (setColor black >> C.paint)
        C.transform (C.Matrix 1 0 0 (-1) 0 picHeight)
        _plotPreview result
        pure ()

gcodeDrawing :: Plot ()
gcodeDrawing = for_ [fromIntegral i *. gridX +. fromIntegral j *. gridY | j <- [0..18], i <- [j `mod` 2, j `mod` 2 + 2 .. 24]] $ \center ->
    for_ [0, 1, 5, 9, 10, 14] $ \i -> do
        let (start1, end1) = arcStartEnd center i
            (end2, start2) = arcStartEnd center (i+0.5)
        repositionTo start1
        clockwiseArcAroundTo center end1
        lineTo start2
        counterclockwiseArcAroundTo center end2
  where
    radius = 25
    gridX = Vec2 radius 0
    gridY = Vec2 0 radius
    arcStartEnd center i =
        let a = radius
            b = radius * sqrt 2
            c = radius - i
            gamma = acos ((a^2 + b^2 - c^2) / (2*a*b))
        in  ( center +. gridY -. gridX +. polar (deg (-45) +. rad gamma) radius
            , center -. gridY -. gridX +. polar (deg 45 -. rad gamma) radius
            )
