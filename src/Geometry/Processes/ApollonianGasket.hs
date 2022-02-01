module Geometry.Processes.ApollonianGasket where



import Data.Complex

import Geometry.Core



data Circle = Circle Vec2 Double
    deriving (Eq, Ord, Show)

-- | Apollon circle, but in a format more suitable for calculation of Apollonian circles.
data ApoCircle = ApoCircle (Complex Double) Double

data Size = SmallerRadius | LargerRadius
    deriving (Eq, Ord, Show)

-- | Curvature of the new Apollonian circle.
newCurvature
    :: Size
    -> Double -- ^ Curvature of first circle
    -> Double -- ^ Second circle
    -> Double -- ^ Third circle
    -> Double -- ^ New circle
newCurvature size k1 k2 k3 =
    -- Taken straight from Wikipedia: https://en.wikipedia.org/wiki/Descartes%27_theorem
    (k1 + k2 + k3) +- (2 * sqrt (k1*k2 + k2*k3 + k3*k1))
  where
    (+-) = case size of
        SmallerRadius -> (+)
        LargerRadius -> (-)

-- | Center of the new Apollonian circle.
--
newCenter
    :: Size
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
    -> Double         -- ^ Curvature of the new circle
    -> Complex Double -- ^ Center of the new circle
newCenter size (ApoCircle c1 k1) (ApoCircle c2 k2) (ApoCircle c3 k3) k4 =
    -- Taken straight from Wikipedia: https://en.wikipedia.org/wiki/Descartes%27_theorem
    1/k4' * ( c1*k1' + c2*k2' + c3*k3'
              +- 2*sqrt (k1'*k2'*c1*c2 + k2'*k3'*c2*c3 + k1'*k3'*c1*c3))
  where
    [k1', k2', k3', k4'] = map (:+ 0) [k1, k2, k3, k4]
    (+-) = case size of
        SmallerRadius -> (+)
        LargerRadius -> (-)

-- | Create a new Apollonian circle, based on three existing ones.
newCircle
    :: Size
        -- ^ Plus (larger curvature, smaller circle) or minus (smaller curvature, larger circle).
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
    -> ApoCircle
newCircle size circ1@(ApoCircle _ k1) circ2@(ApoCircle _ k2) circ3@(ApoCircle _ k3) =
    let k4 = newCurvature size k1 k2 k3
        c4 = newCenter (error "Plus or minus here? This is independent of the size parameter of newCurvature I think") circ1 circ2 circ3 k4
    in ApoCircle c4 k4

toCircle :: ApoCircle -> Circle
toCircle (ApoCircle (x :+ y) k) = Circle (Vec2 x y) (1/k)

toApoCircle :: Circle -> ApoCircle
toApoCircle (Circle (Vec2 x y) r) = ApoCircle (x :+ y) (1/r)
