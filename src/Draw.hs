module Draw where



import Control.Monad
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Foldable
import Data.Semigroup
import Data.Time.Clock.POSIX
import Graphics.Rendering.Cairo
import System.Random

import Geometry

lineSketch :: Line -> Render ()
lineSketch (Line (Vec2 x1 y1) (Vec2 x2 y2)) = moveTo x1 y1 >> lineTo x2 y2

circleSketch :: Vec2 -> Distance -> Render ()
circleSketch (Vec2 x y) (Distance r) = arc x y r 0 (2*pi)

crossSketch :: Vec2 -> Distance -> Render ()
crossSketch center (Distance r) = do
    let lowerRight = rotateAround center (deg 45) (center `addVec2` Vec2 r 0)
        line1 = angledLine lowerRight (deg (45+180)) (Distance (2*r))
        line2 = rotateAround center (deg 90) line1
    lineSketch line1
    lineSketch line2

arcSketch :: Vec2 -> Distance -> Angle -> Angle -> Render ()
arcSketch (Vec2 x y) (Distance r) (Angle angleStart) (Angle angleEnd)
  = arc x y r angleStart angleEnd
