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
    let RunPlotResult{..} = runPlot settings gcodeDrawing
    writeGCodeFile "fish-scales-pattern.g" _plotGCode
    render "out/fish-scales-pattern.png" picWidth picHeight $ do
        cairoScope (setColor black >> C.paint)
        C.transform (C.Matrix 1 0 0 (-1) 0 picHeight)
        _plotPreview
        pure ()

gcodeDrawing :: Plot ()
gcodeDrawing = do
    let radius = 50
        gridX = Vec2 radius 0
        gridY = Vec2 0 radius
    for_ [(2 * x + y `mod` 2, y) | x <- [0..6], y <- [0..9]] $ \(x, y) -> do
        let center = fromIntegral x *. gridX +. fromIntegral y *. gridY
        for_ [0, 15, 20, 25, 29] $ \i -> do
            let grid'  = gridX -. Vec2 i 0
                grid'' = gridX -. Vec2 (i + 0.5) 0
                (start, end) =
                    let a = radius
                        b = radius * sqrt 2
                        c = radius - i
                        gamma = acos ((a^2 + b^2 - c^2) / (2*a*b))
                    in ( center -. gridX -. gridY +. polar (deg 45 +. rad gamma) radius
                       , center +. gridX -. gridY +. polar (deg 135 -. rad gamma) radius
                       )

            repositionTo start
            clockwiseArcAroundTo center end
            lineTo (end -. Vec2 0 0.5)
            counterclockwiseArcAroundTo center (start +. Vec2 0 0.5)
