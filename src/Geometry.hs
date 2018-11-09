module Geometry where



import Data.Maybe
import System.Random



data Vec2 = Vec2 !Double !Double deriving (Eq, Ord, Show)
newtype Polygon = Polygon [Vec2] deriving (Eq, Ord, Show)
data Circle = Circle Vec2 !Double deriving (Eq, Ord, Show)
data Line = Line Vec2 Vec2 deriving (Eq, Ord, Show)
newtype Angle = Angle Double deriving (Eq, Ord, Show)
newtype Distance = Distance Double deriving (Eq, Ord, Show)

instance Random Vec2 where
    randomR (lo, hi) gen
      = let Vec2 loX loY = lo
            (x, gen')  = randomR (loX, hiX) gen
            Vec2 hiX hiY = hi
            (y, gen'') = randomR (loY, hiY) gen'
        in (Vec2 x y, gen'')
    random gen
      = let (x, gen')  = random gen
            (y, gen'') = random gen'
        in (Vec2 x y, gen'')


class Move geo where
    move :: Vec2 -> geo -> geo

moveRad :: Move geo => Angle -> Distance -> geo -> geo
moveRad (Angle a) (Distance d) = move (Vec2 (d * cos a) (d * sin a))

instance Move Vec2 where
    move = addVec2

instance Move Polygon where
    move offset (Polygon points) = Polygon (map (move offset) points)

instance Move Circle where
    move offset (Circle center r) = Circle (move offset center) r

instance Move Line where
    move offset (Line a b) = Line (move offset a) (move offset b)

instance Move geo => Move [geo] where
    move offset = fmap (move offset)

instance Move geo => Move (Maybe geo) where
    move offset = fmap (move offset)

class Rotate geo where
    rotateAround :: Vec2 -> Angle -> geo -> geo

instance Rotate Vec2 where
    rotateAround (Vec2 rx ry) (Angle angle) (Vec2 px py) = Vec2 px' py'
      where
        px0 = px - rx
        py0 = py - ry

        pxR = px0 * cos angle - py0 * sin angle
        pyR = px0 * sin angle + py0 * cos angle

        px' = pxR + rx
        py' = pyR + ry

instance Rotate Line where
    rotateAround pivot angle (Line p1 p2)
      = Line (rotateAround pivot angle p1) (rotateAround pivot angle p2)

instance Rotate Polygon where
    rotateAround pivot angle (Polygon points)
      = Polygon (map (rotateAround pivot angle) points)

instance Rotate Circle where
    rotateAround pivot angle (Circle center r) = Circle (rotateAround pivot angle center) r

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)

negateVec2 :: Vec2 -> Vec2
negateVec2 (Vec2 x y) = Vec2 (-x) (-y)

subtractVec2 :: Vec2 -> Vec2 -> Vec2
subtractVec2 v1 v2 = addVec2 v1 (negateVec2 v2)

dotProduct :: Vec2 -> Vec2 -> Double
dotProduct (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

norm :: Vec2 -> Distance
norm v = Distance (sqrt (dotProduct v v))

angleL :: Line -> Angle
angleL (Line (Vec2 x1 y1) (Vec2 x2 y2)) = Angle (atan2 (y2-y1) (x2-x1))

angledLine :: Vec2 -> Angle -> Distance -> Line
angledLine start@(Vec2 x y) (Angle angle) (Distance len) = Line start end
  where
    end = Vec2 (x + len * cos angle) (y + len * sin angle)

data LLIntersection
    = Intersection
    | VirtualIntersectionL
    | VirtualIntersectionR
    | VirtualIntersection
    deriving (Eq, Ord, Show)

intersectionLL :: Line -> Line -> (Vec2, Angle, LLIntersection)
intersectionLL line1 line2 = (iPoint, iAngle, iType)
  where
    iType = case (virtualL, virtualR) of
        (True,  True)  -> VirtualIntersection
        (True,  False) -> VirtualIntersectionL
        (False, True)  -> VirtualIntersectionR
        (False, False) -> Intersection

    Line (Vec2 x1 y1) (Vec2 x2 y2) = line1
    Line (Vec2 a1 b1) (Vec2 a2 b2) = line2

    iPoint = Vec2 (x1 + t * (x2-x1)) (y1 + t * (y2-y1))
    t =   ((x1-a1) * (b1-b2) - (y1-b1) * (a1-a2))
        / ---------------------------------------
          ((x1-x2) * (b1-b2) - (y1-y2) * (a1-a2))
    virtualL = t < 0 || t > 1

    u = - ((x1-x2) * (y1-b1) - (y1-y2) * (x1-a1))
        / ---------------------------------------
          ((x1-x2) * (b1-b2) - (y1-y2) * (a1-a2))
    virtualR = u < 0 || u > 1

    iAngle = let Angle a1 = angleL line1
                 Angle a2 = angleL line2
             in Angle (a2 - a1)

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

    intersections = flip filter edges (\edge ->
        case intersectionLL testRay edge of
            (_, _, Intersection) -> True
            _other -> False)

    edges = polygonEdges poly
