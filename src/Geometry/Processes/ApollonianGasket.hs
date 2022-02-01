module Geometry.Processes.ApollonianGasket where



import Data.Complex

import Geometry.Core



data Circle = Circle Vec2 Double
    deriving (Eq, Ord, Show)

-- | Apollon circle, but in a format more suitable for calculation of Apollonian circles.
data ApoCircle = ApoCircle (Complex Double) Double
    deriving (Eq, Show)

data Size = SmallerRadius | LargerRadius
    deriving (Eq, Ord, Show)

-- | Curvature of the new Apollonian circle.
newCurvature
    :: (Double -> Double -> Double)
    -> Double -- ^ Curvature of first circle
    -> Double -- ^ Second circle
    -> Double -- ^ Third circle
    -> Double -- ^ New circle
newCurvature (+-) k1 k2 k3 =
    -- Taken straight from Wikipedia: https://en.wikipedia.org/wiki/Descartes%27_theorem
    (k1 + k2 + k3) +- (2 * sqrt (k1*k2 + k2*k3 + k3*k1))

-- | Center of the new Apollonian circle.
--
newCenter
    :: (Complex Double -> Complex Double -> Complex Double) -- ^ Plus or minus to yield both circles
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
    -> Double         -- ^ Curvature of the new circle
    -> Complex Double -- ^ Center of the new circle
newCenter (+-) (ApoCircle c1 k1) (ApoCircle c2 k2) (ApoCircle c3 k3) k4 =
    -- Taken straight from Wikipedia: https://en.wikipedia.org/wiki/Descartes%27_theorem
    1/k4' * ( (c1*k1' + c2*k2' + c3*k3')
              +- (2*sqrt (k1'*k2'*c1*c2 + k2'*k3'*c2*c3 + k1'*k3'*c1*c3)))
  where
    [k1', k2', k3', k4'] = map (:+ 0) [k1, k2, k3, k4]

-- | Create a new Apollonian circle, based on three existing ones.
newCircle
    :: (Double -> Double -> Double)
        -- ^ Plus (larger curvature, smaller circle) or minus (smaller curvature, larger circle).
    -> (Complex Double -> Complex Double -> Complex Double)
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
newCircle plusMinusCurvature plusMinusCenter circ1@(ApoCircle _ k1) circ2@(ApoCircle _ k2) circ3@(ApoCircle _ k3) =
    let k4 = newCurvature plusMinusCurvature k1 k2 k3
        c4 = newCenter plusMinusCenter circ1 circ2 circ3 k4
    in ApoCircle c4 k4

toCircle :: ApoCircle -> Circle
toCircle (ApoCircle (x :+ y) k) = Circle (Vec2 x y) (1/k)

toApoCircle :: Circle -> ApoCircle
toApoCircle (Circle (Vec2 x y) r) = ApoCircle (x :+ y) (1/r)
