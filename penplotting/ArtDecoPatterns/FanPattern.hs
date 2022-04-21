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
    let radius = 50
        gridX = Vec2 radius 0
        gridY = Vec2 0 radius
    for_ [(2 * x + y `mod` 2, y) | x <- [0..6], y <- [0..9]] $ \(x, y) -> do
        let center = fromIntegral x *. gridX +. fromIntegral y *. gridY
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
