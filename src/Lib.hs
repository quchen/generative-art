module Lib where



import Data.Maybe



data Vec2 = Vec2 !Double !Double deriving (Eq, Ord, Show)
newtype Polygon = Polygon [Vec2] deriving (Eq, Ord, Show)
data Circle = Circle Vec2 !Double deriving (Eq, Ord, Show)
data Line = Line Vec2 Vec2 deriving (Eq, Ord, Show)


class Move stuff where
    move :: Vec2 -> stuff -> stuff

instance Move Vec2 where
    move = addVec2

instance Move Polygon where
    move offset (Polygon points) = Polygon (map (move offset) points)

instance Move Circle where
    move offset (Circle center r) = Circle (move offset center) r

instance Move geo => Move [geo] where
    move offset = fmap (move offset)

instance Move geo => Move (Maybe geo) where
    move offset = fmap (move offset)

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)

negateVec2 :: Vec2 -> Vec2
negateVec2 (Vec2 x y) = Vec2 (-x) (-y)

subtractVec2 :: Vec2 -> Vec2 -> Vec2
subtractVec2 v1 v2 = addVec2 v1 (negateVec2 v2)

dotProduct :: Vec2 -> Vec2 -> Double
dotProduct (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

norm :: Vec2 -> Double
norm v = sqrt (dotProduct v v)


data LLIntersection = Intersection | VirtualIntersection
    deriving (Eq, Ord, Show)

intersectionLL :: Line -> Line -> (Vec2, LLIntersection)
intersectionLL (Line (Vec2 x1 y1) (Vec2 x2 y2)) (Line (Vec2 x3 y3) (Vec2 x4 y4))
    | t < 0 || t > 1 = (p, VirtualIntersection)
    | otherwise      = (p, Intersection)
  where
    p = Vec2 (x1 + t * (x2-x1)) (y1 + t * (y2-y1))
    t = ((x1-x3) * (y3-y4) - (y1-y3) * (x3-x4))
        / -------------------------------------
        ((x1-x2) * (y3-y4) - (y1-y3) * (x3-x4))

intersectionLC :: Line -> Circle -> Maybe (Vec2, Vec2)
intersectionLC (Line (Vec2 x1 y1) (Vec2 x2 y2)) (Circle _ r) = undefined
  where
    -- Taken from MathWorld,
    -- http://mathworld.wolfram.com/Circle-LineIntersection.html
    dx = x2 - x1
    dy = y2 - y1
    dr = sqrt (dx^2 + dy^2)
    dd = x1*y2 - x2*y1
    sgnStar x = if x < 0 then -1 else 1
    delta = r^2 * dr^2 - dd^2

polygonEdges :: Polygon -> [Line]
polygonEdges (Polygon ps) = zipWith Line ps (drop 1 (cycle ps))

-- Ray-casting algorithm.
pointInPolygon :: Vec2 -> Polygon -> Bool
pointInPolygon p poly = odd (length intersections)
  where
    -- The test ray comes from outside the polygon from the left, and ends at
    -- the point to be tested.
    testRay = Line (Vec2 (leftmostPolyX - 1) pointY) p
      where
        leftmostPolyX = minimum (edges >>= \(Line (Vec2 x1 _) (Vec2 x2 _)) -> [x1,x2])
        Vec2 _ pointY = p

    intersections = filter ((== Intersection) . snd . intersectionLL testRay) edges

    edges = polygonEdges poly
