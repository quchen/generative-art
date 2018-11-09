module Draw where



import Graphics.Rendering.Cairo
import System.Random
import Data.Foldable
import Control.Monad
import Data.Time.Clock.POSIX
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Semigroup

import Geometry

lineSketch :: Line -> Render ()
lineSketch (Line (Vec2 x1 y1) (Vec2 x2 y2)) = moveTo x1 y1 >> lineTo x2 y2

circleSketch :: Vec2 -> Distance -> Render ()
circleSketch (Vec2 x y) (Distance r) = arc x y r 0 (2*pi)

arcSketch :: Vec2 -> Distance -> Angle -> Angle -> Render ()
arcSketch (Vec2 x y) (Distance r) (Angle angleStart) (Angle angleEnd)
  = arc x y r angleStart angleEnd
