module Main (main) where



import Data.Default.Class
import Data.Foldable
import Draw
import Draw.Plotting
import Geometry           as G



main :: IO ()
main = do
    let settings = def {
            _feedrate = 500
        }
        plotResult = runPlot settings $ do
            for_ (_nonius calibrationDrawing) plot
            for_ (_circles calibrationDrawing) plot
            for_ (_crosshair calibrationDrawing) plot
    writeGCodeFile "out/calibration.g" plotResult
    renderPreview "out/preview.svg" plotResult

data CalibrationDrawing = CalibrationDrawing
    { _nonius :: [Line]
    , _circles :: [Circle]
    , _crosshair :: [Line]
    } deriving (Eq, Ord, Show)

calibrationDrawing :: CalibrationDrawing
calibrationDrawing =
    let xNonius = G.transform (G.translate (Vec2 (-25) (-35))) xAxisNonius
        yNonius = G.transform (G.rotateAround zero (deg 90) <> G.translate (Vec2 (-25) (-35))) xAxisNonius
        circles = [Circle zero r | r <- scanl (+) 10 [1..5]]

        crosshairDiameter = 8
        crosshair = do
            angle <- [deg 0, deg 45, deg 90, deg 135]
            let horizontalLine = Line (Vec2 0 (-crosshairDiameter)) (Vec2 0 crosshairDiameter)
            pure (G.transform (rotateAround zero angle) horizontalLine)
    in CalibrationDrawing
        { _nonius = xNonius <> yNonius
        , _circles = circles
        , _crosshair = crosshair
        }

xAxisNonius :: [Line]
xAxisNonius =
    let yOffset = 10
        xs = [5, 10 .. 50]
        offsets = map (/10) [1..]
        brokenBars = concat $ zipWith
            (\x dx ->
                [ Line (Vec2 (x+dx) 0) (Vec2 (x+dx) (-yOffset))
                , Line (Vec2 x (-yOffset/5)) (Vec2 x (yOffset/5))
                , Line (Vec2 (x-dx) 0) (Vec2 (x-dx) yOffset) ]
                )
            xs
            offsets
        fullBar = Line (Vec2 zero (-yOffset)) (Vec2 zero yOffset)
    in fullBar : brokenBars
