module Main (main) where



import Control.Monad.State.Class
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
            , _feedrate = 4000
            , _previewPenWidth = 0.5
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
gcodeDrawing = for_ [fromIntegral i *. gridX +. fromIntegral j *. gridY | j <- [1..17], i <- [j `mod` 2 + 1, j `mod` 2 + 3 .. 23]] $ \center ->
    for_ [0, 0.5, 3, 3.5, 4, 4.5, 5, 7.5, 10, 10.5, 11, 11.5, 12, 14] $ \i -> do
        penPos <- gets _penXY
        let (a, b) = arcStartEnd center i
            (start, drawArc) = if norm (penPos -. a) < norm (penPos -. b)
                then (a, clockwiseArcAroundTo center b)
                else (b, counterclockwiseArcAroundTo center a)
        if norm (penPos -. start) < 1
            then lineTo start
            else repositionTo start
        drawArc
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
