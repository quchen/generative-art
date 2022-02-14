module Main (main) where



import Data.Foldable
import Graphics.Rendering.Cairo as C hiding (height, width, x, y)

import Draw
import Geometry               as G
import Numerics.Interpolation



width :: Num a => a
width = 240

height :: Num a => a
height = 800

main :: IO ()
main = do
    withSurfaceAuto "out/dna-helix.svg" width height (\surface -> renderWith surface renderDrawing)
    withSurfaceAuto "out/dna-helix.png" width height (\surface -> renderWith surface renderDrawing)

data DnaLine = DnaLine Double Double Double
    deriving (Eq, Ord, Show)

data Acid = C | G | A | T
    deriving (Eq, Ord, Show, Enum)

data HelixPoint = HelixPoint Vec2 Double

instance VectorSpace HelixPoint where
    HelixPoint xy1 z1 +. HelixPoint xy2 z2 = HelixPoint (xy1+.xy2) (z1+.z2)
    HelixPoint xy1 z1 -. HelixPoint xy2 z2 = HelixPoint (xy1-.xy2) (z1-.z2)
    a *. HelixPoint xy z = HelixPoint (a *. xy) (a *. z)
    zero = HelixPoint zero zero

renderDrawing :: Render ()
renderDrawing = do
    -- cairoScope $ grouped (paintWithAlpha 0.5) cartesianCoordinateSystem
    setLineWidth 1
    let basePairs = map makeBasePair [1..]
        yOfBasePair (HelixPoint (Vec2 _ y) _, _) = y
    for_ (takeWhile (\bp -> yOfBasePair bp <= height - 16) basePairs) $ \(HelixPoint base1 radius1, HelixPoint base2 radius2) -> do

        circleSketch base1 radius1 >> stroke
        lineSketch (Line base1 base2) >> stroke
        circleSketch base2 radius2 >> stroke

makeBasePair :: Double -> (HelixPoint, HelixPoint)
makeBasePair basePairIx =
    let yRotation = deg 25
        ySpacing = 16
        artisticFreedomSpacing = 2

        helix1Angle = deg (basePairIx*32.7 / artisticFreedomSpacing) +. yRotation
        helix1x =   (width/2-16) * sin (getRad helix1Angle) + width/2
        helix1z = - (width/2-16) * cos (getRad helix1Angle)

        helix2Angle = helix1Angle +. deg 120
        helix2x =   (width/2-16) * sin (getRad helix2Angle) + width/2
        helix2z = - (width/2-16) * cos (getRad helix2Angle)

        y = basePairIx * ySpacing

        radius = linearInterpolate (-100, 100) (5, 9)
    in (HelixPoint (Vec2 helix1x y) (radius helix1z), HelixPoint (Vec2 helix2x y) (radius helix2z))
