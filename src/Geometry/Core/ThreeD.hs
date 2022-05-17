module Geometry.Core.ThreeD where



import Control.DeepSeq
import qualified System.Random.MWC as MWC

import Algebra.VectorSpace
import Geometry.Core (Vec2(..))


data Vec3 = Vec3 !Double !Double !Double deriving (Eq, Ord, Show)

instance NFData Vec3 where rnf _ = ()

instance MWC.UniformRange Vec3 where
    uniformRM (Vec3 xMin yMin zMin, Vec3 xMax yMax zMax) gen = Vec3
        <$> MWC.uniformRM (xMin, xMax) gen
        <*> MWC.uniformRM (yMin, yMax) gen
        <*> MWC.uniformRM (zMin, zMax) gen

-- | Projection of a 3D vector into the 2D x-y plane
zProjection :: Vec3 -> Vec2
zProjection (Vec3 x y _) = Vec2 x y

instance VectorSpace Vec3 where
    Vec3 x1 y1 z1 +. Vec3 x2 y2 z2 = Vec3 (x1+x2) (y1+y2) (z1+z2)
    a *. Vec3 x y z = Vec3 (a*x) (a*y) (a*z)
    negateV (Vec3 x y z) = Vec3 (-x) (-y) (-z)
    zero = Vec3 0 0 0

dotProduct :: Vec3 -> Vec3 -> Double
dotProduct (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- | Euclidean norm.
--
-- \[ \|\mathbf v\| = \sqrt{v_x^2 + v_y^2 + v_z^2} \]
norm :: Vec3 -> Double
norm = sqrt . normSquare

-- | Squared Euclidean norm. Does not require a square root, and is thus
-- suitable for sorting points by distance without excluding certain kinds of
-- numbers such as rationals.
--
-- \[ \|\mathbf v\|^2 = v_x^2 + v_y^2 + v_z^2 \]
normSquare :: Vec3 -> Double
normSquare v = dotProduct v v
