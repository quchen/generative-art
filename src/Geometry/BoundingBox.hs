module Geometry.BoundingBox (HasBoundingBox(..), BoundingBox(..)) where

import Geometry.Core

data BoundingBox = BoundingBox !Vec2 !Vec2

instance Semigroup BoundingBox where
    BoundingBox (Vec2 xMin1 yMin1) (Vec2 xMax1 yMax1) <> BoundingBox (Vec2 xMin2 yMin2) (Vec2 xMax2 yMax2)
      = BoundingBox (Vec2 (min xMin1 xMin2) (min yMin1 yMin2))
                    (Vec2 (max xMax1 xMax2) (max yMax1 yMax2))

instance Monoid BoundingBox where
    mempty = BoundingBox (Vec2 inf inf) (Vec2 (-inf) (-inf))
      where inf = 1/0

class HasBoundingBox a where
    boundingBox :: a -> BoundingBox

instance HasBoundingBox BoundingBox where
    boundingBox = id

instance HasBoundingBox Vec2 where
    boundingBox v = BoundingBox v v

instance (HasBoundingBox a, HasBoundingBox b) => HasBoundingBox (a,b) where
    boundingBox (a,b) = boundingBox a <> boundingBox b

instance (HasBoundingBox a, HasBoundingBox b, HasBoundingBox c) => HasBoundingBox (a,b,c) where
    boundingBox (a,b,c) = boundingBox a <> boundingBox b <>  boundingBox c

instance HasBoundingBox a => HasBoundingBox [a] where
    boundingBox = foldMap boundingBox

instance HasBoundingBox Line where
    boundingBox (Line start end) = boundingBox (start, end)

instance HasBoundingBox Polygon where
    boundingBox (Polygon ps) = boundingBox ps

instance HasBoundingBox vec => HasBoundingBox (Bezier vec) where
    boundingBox (Bezier a b c d) = boundingBox [a,b,c,d]
