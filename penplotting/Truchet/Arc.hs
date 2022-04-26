module Arc where



import Control.Monad.State.Class

import Draw
import Draw.Plotting
import Geometry



data Arc
    = CwArc Vec2 Vec2 Vec2
    | CcwArc Vec2 Vec2 Vec2
    | Straight Vec2 Vec2
    deriving (Eq, Ord, Show)

arcStartEnd :: Arc -> (Vec2, Vec2)
arcStartEnd = \case
    CwArc _ start end -> (start, end)
    CcwArc _ start end -> (start, end)
    Straight start end -> (start, end)

reverseArc :: Arc -> Arc
reverseArc = \case
    CwArc center start end -> CcwArc center end start
    CcwArc center start end -> CwArc center end start
    Straight start end -> Straight end start

cwArc :: Vec2 -> Double -> Angle -> Angle -> Arc
cwArc center radius startAngle endAngle = CwArc center start end
  where
    start = center +. polar startAngle radius
    end = center +. polar endAngle radius

ccwArc :: Vec2 -> Double -> Angle -> Angle -> Arc
ccwArc center radius startAngle endAngle = CcwArc center start end
  where
    start = center +. polar startAngle radius
    end = center +. polar endAngle radius

straight :: Vec2 -> Vec2 -> Arc
straight start end = Straight start end

-- Valid for translation, rotation and aspect-preserving scaling,
-- but breaks down for mirroring and not-aspect-preserving scaling.
instance Transform Arc where
    transform t (CwArc center start end) = CwArc (transform t center) (transform t start) (transform t end)
    transform t (CcwArc center start end) = CcwArc (transform t center) (transform t start) (transform t end)
    transform t (Straight start end) = Straight (transform t start) (transform t end)

instance Sketch Arc where
    sketch (CwArc center start end) = do
        let radius = norm (start -. center)
            startAngle = angleOfLine (Line center start)
            endAngle = angleOfLine (Line center end)
        arcSketchNegative center radius startAngle endAngle
    sketch (CcwArc center start end) = do
        let radius = norm (start -. center)
            startAngle = angleOfLine (Line center start)
            endAngle = angleOfLine (Line center end)
        arcSketch center radius startAngle endAngle
    sketch (Straight start end) = sketch (Line start end)

instance Plotting Arc where
    plot (CwArc center start end) = do
        pos <- gets _penXY
        case norm (pos -. start) of
            0 -> pure ()
            d | d < 0.1 -> lineTo start
            _otherwise -> repositionTo start
        clockwiseArcAroundTo center end
    plot (CcwArc center start end) = do
        pos <- gets _penXY
        case norm (pos -. start) of
            0 -> pure ()
            d | d < 0.1 -> lineTo start
            _otherwise -> repositionTo start
        counterclockwiseArcAroundTo center end
    plot (Straight start end) = do
        pos <- gets _penXY
        case norm (pos -. start) of
            d | d < 0.1 -> lineTo end
            _otherwise -> repositionTo start >> lineTo end
