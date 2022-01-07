module Geometry.BoundingBox (HasBoundingBox(..), BoundingBox(..)) where

import Geometry.Core
import Data.List

-- | The bounding box, with the minimum and maximum vectors.
--
-- In geometrical terms, the bounding box is a rectangle spanned by the top-left
-- (minimum) and bottom-right (maximum) points, so that everything is inside the
-- rectangle.
--
-- Make sure the first argument is smaller than the second when using the
-- constructor directly! Or better yet, don’t use the constructor and create
-- bounding boxes via the provided instances.
data BoundingBox = BoundingBox !Vec2 !Vec2

instance Semigroup BoundingBox where
    BoundingBox (Vec2 xMin1 yMin1) (Vec2 xMax1 yMax1) <> BoundingBox (Vec2 xMin2 yMin2) (Vec2 xMax2 yMax2)
      = BoundingBox (Vec2 (min xMin1 xMin2) (min yMin1 yMin2))
                    (Vec2 (max xMax1 xMax2) (max yMax1 yMax2))

-- | A bounding box with the minimum at (plus!) infinity and maximum at (minus!)
-- infinity acts as a neutral element. This is mostly useful so we can make
-- potentiallly empty data structures such as @[a]@ and @'Maybe' a@ instances too.
instance Monoid BoundingBox where
    mempty = BoundingBox (Vec2 inf inf) (Vec2 (-inf) (-inf))
      where inf = 1/0

-- | Anything we can paint has a bounding box. Knowing it is useful to e.g. rescale
-- the geometry to fit into the canvas or for collision detection.
class HasBoundingBox a where
    boundingBox :: a -> BoundingBox

instance HasBoundingBox BoundingBox where
    boundingBox = id

instance HasBoundingBox Vec2 where
    boundingBox v = BoundingBox v v

instance (HasBoundingBox a, HasBoundingBox b) => HasBoundingBox (a,b) where
    boundingBox (a,b) = boundingBox a <> boundingBox b

instance (HasBoundingBox a, HasBoundingBox b, HasBoundingBox c) => HasBoundingBox (a,b,c) where
    boundingBox (a,b,c) = boundingBox (a,b) <> boundingBox c

instance (HasBoundingBox a, HasBoundingBox b, HasBoundingBox c, HasBoundingBox d) => HasBoundingBox (a,b,c,d) where
    boundingBox (a,b,c,d) = boundingBox (a,b) <> boundingBox (c,d)

instance (HasBoundingBox a, HasBoundingBox b, HasBoundingBox c, HasBoundingBox d, HasBoundingBox e) => HasBoundingBox (a,b,c,d,e) where
    boundingBox (a,b,c,d,e) = boundingBox (a,b) <> boundingBox (c,d,e)

instance HasBoundingBox a => HasBoundingBox (Maybe a) where
    boundingBox = foldMap boundingBox

instance HasBoundingBox a => HasBoundingBox [a] where
    boundingBox = foldMap boundingBox

instance HasBoundingBox Line where
    boundingBox (Line start end) = boundingBox (start, end)

instance HasBoundingBox Polygon where
    boundingBox (Polygon ps) = boundingBox ps

instance HasBoundingBox vec => HasBoundingBox (Bezier vec) where
    boundingBox (Bezier a b c d) = boundingBox (a,b,c,d)
