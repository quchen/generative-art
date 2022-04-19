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
    render "out/fan-pattern.png" picWidth picHeight cairoDrawing
    render "out/fan-pattern.svg" picWidth picHeight cairoDrawing
    let settings = def
            { _previewPenWidth = 0.5
            , _previewPenTravelColor = Nothing
            , _previewDecorate = False
            }
    let RunPlotResult{..} = runPlot settings gcodeDrawing
    writeGCodeFile "fan-pattern.g" _plotGCode
    render "out/fan-pattern.png" picWidth picHeight $ do
        cairoScope (setColor black >> C.paint)
        C.transform (C.Matrix 1 0 0 (-1) 0 picHeight)
        _plotPreview
        pure ()

cairoDrawing :: C.Render ()
cairoDrawing = cairoScope $ do
    cairoScope (setColor white >> C.paint)
    setColor black
    C.setLineWidth 0.8
    let radius = 50
        gridX = Vec2 radius 0
        gridY = Vec2 0 radius
    for_ [(2 * x + y `mod` 2, y) | x <- [0..6], y <- [0..9]] $ \(x, y) -> do
        let center = fromIntegral x *. gridX +. fromIntegral y *. gridY
        arcSketchNegative center radius (deg 0) (deg 180)
        C.stroke
        arcSketchNegative center (radius - 2) (deg 0) (deg 180)
        C.stroke
        for_ (deg <$> [12, 24..168]) $ \a -> do
            let startPoint = center +. gridY
                pointOnArc = center +. (radius - 2) *. Vec2 (cos (getRad a)) (-sin (getRad a))
                center' = case intersectionLL (angledLine startPoint (deg 0) 1) (perpendicularBisector (Line startPoint pointOnArc)) of
                    IntersectionReal p -> p
                    IntersectionVirtual p -> p
                    IntersectionVirtualInsideL p -> p
                    IntersectionVirtualInsideR p -> p
                    _otherwise -> error "Lines must intersect"
                radius' = norm (center' -. startPoint)
                (startAngle, endAngle) =
                    let Vec2 dx dy = (pointOnArc -. center')
                        isNegativeAngle = dx > 0
                    in if isNegativeAngle
                        then (rad (atan2 dy dx), deg 0)
                        else (deg 180, rad (atan2 dy dx))
            arcSketch center' radius' startAngle endAngle
            C.stroke

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
        for_ (deg <$> [12, 24 .. 168]) $ \a -> do
            let startPoint = center -. gridY
                pointOnArc = center +. (radius - 2) *. Vec2 (-cos (getRad a)) (sin (getRad a))
                center' = case intersectionLL (angledLine startPoint (deg 0) 1) (perpendicularBisector (Line startPoint pointOnArc)) of
                    IntersectionReal p -> p
                    IntersectionVirtual p -> p
                    IntersectionVirtualInsideL p -> p
                    IntersectionVirtualInsideR p -> p
                    _otherwise -> error "Lines must intersect"
            repositionTo startPoint
            if getDeg a < 90
                then counterclockwiseArcAroundTo center' pointOnArc
                else clockwiseArcAroundTo center' pointOnArc
            pure ()
