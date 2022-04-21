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
        result = runPlot settings gcodeDrawing
    writeGCodeFile "fan-pattern.g" result
    render "out/fan-pattern.png" picWidth picHeight $ do
        cairoScope (setColor black >> C.paint)
        C.transform (C.Matrix 1 0 0 (-1) 0 picHeight)
        _plotPreview result
        pure ()

gcodeDrawing :: Plot ()
gcodeDrawing = do
    let radius = 25
        gridX = Vec2 radius 0
        gridY = Vec2 0 radius
    for_ [fromIntegral x *. gridX +. fromIntegral y *. gridY | y <- [0..18], x <- [y `mod` 2, y `mod` 2 + 2 .. 24]] $ \center -> do
        repositionTo (center -. gridX)
        clockwiseArcAroundTo center (center +. gridX)
        repositionTo (center +. (gridX -. Vec2 2 0))
        counterclockwiseArcAroundTo center (center -. (gridX -. Vec2 2 0))
        for_ (zip (deg <$> [12, 24 .. 84]) (cycle [True, False])) $ \(alpha, clockwise) -> do
            let startPoint = center -. gridY
                pointOnArc = center +. (radius - 2) *. Vec2 (-cos (getRad alpha)) (sin (getRad alpha))
                center' = case intersectionLL (angledLine startPoint (deg 0) 1) (perpendicularBisector (Line startPoint pointOnArc)) of
                    IntersectionReal p -> p
                    IntersectionVirtual p -> p
                    IntersectionVirtualInsideL p -> p
                    IntersectionVirtualInsideR p -> p
                    _otherwise -> error "Lines must intersect"
                (mirrorCenter, mirrorPointOnArc) = transform (mirrorAlong (angledLine startPoint (deg 90) 1)) (center', pointOnArc)
            if clockwise
                then do
                    repositionTo pointOnArc
                    clockwiseArcAroundTo center' startPoint
                    clockwiseArcAroundTo mirrorCenter mirrorPointOnArc
                else do
                    repositionTo mirrorPointOnArc
                    counterclockwiseArcAroundTo mirrorCenter startPoint
                    counterclockwiseArcAroundTo center' pointOnArc
