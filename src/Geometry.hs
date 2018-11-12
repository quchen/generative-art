{-# LANGUAGE BangPatterns #-}

module Geometry where



import Control.Monad
import Data.Fixed
import Data.List



data Vec2 = Vec2 !Double !Double deriving (Eq, Ord, Show)
newtype Polygon = Polygon [Vec2] deriving (Eq, Ord, Show)
data Circle = Circle Vec2 !Double deriving (Eq, Ord, Show)
data Line = Line Vec2 Vec2 deriving (Eq, Ord, Show)
newtype Angle = Angle Double deriving (Eq, Ord, Show)
newtype Distance = Distance Double deriving (Eq, Ord, Show)
newtype Area = Area Double deriving (Eq, Ord, Show)

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

class Mirror geo where
    mirrorAlong :: Line -> geo -> geo

instance Mirror Vec2 where
    mirrorAlong mirror@(Line start _) p = p'
      where
        virtual = Line start p
        Angle angle = angleBetween mirror virtual
        Line _ p' = rotateAround start (Angle (-2*angle)) virtual

instance Mirror Line where
    mirrorAlong mirror (Line start end) = Line (mirrorAlong mirror start)
                                               (mirrorAlong mirror end)

instance Mirror Polygon where
    mirrorAlong mirror (Polygon ps) = Polygon (map (mirrorAlong mirror) ps)

addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)

negateVec2 :: Vec2 -> Vec2
negateVec2 (Vec2 x y) = Vec2 (-x) (-y)

subtractVec2 :: Vec2 -> Vec2 -> Vec2
subtractVec2 v1 v2 = addVec2 v1 (negateVec2 v2)

mulVec2 :: Double -> Vec2 -> Vec2
mulVec2 a (Vec2 x y) = Vec2 (a*x) (a*y)

dotProduct :: Vec2 -> Vec2 -> Double
dotProduct (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

norm :: Vec2 -> Distance
norm v = Distance (sqrt (dotProduct v v))

angleOfLine :: Line -> Angle
angleOfLine (Line (Vec2 x1 y1) (Vec2 x2 y2)) = Angle (atan2 (y2-y1) (x2-x1))

angleBetween :: Line -> Line -> Angle
angleBetween line1 line2
  = let Angle a1 = angleOfLine line1
        Angle a2 = angleOfLine line2
    in Angle (a2 - a1)

angledLine :: Vec2 -> Angle -> Distance -> Line
angledLine start@(Vec2 x y) (Angle angle) (Distance len) = Line start end
  where
    end = Vec2 (x + len * cos angle) (y + len * sin angle)

lineLength :: Line -> Distance
lineLength (Line start end) = norm (end `subtractVec2` start)

resizeLine :: Line -> (Distance -> Distance) -> Line
resizeLine line@(Line start _end) f
  = angledLine start (angleOfLine line) (f (lineLength line))

-- | Move the end point of the line so that it has length 1.
normalizeLine :: Line -> Line
normalizeLine line = resizeLine line (const (Distance 1))

-- | Switch defining points of a line
lineReverse :: Line -> Line
lineReverse (Line start end) = Line end start

data LLIntersection
    = IntersectionReal
    | IntersectionVirtualInsideL
    | IntersectionVirtualInsideR
    | IntersectionVirtual
    deriving (Eq, Ord, Show)

-- | Calculate the intersection of two lines.
--
-- Returns the point and angle of the intersection, and whether it is inside
-- both, one, or none of the provided finite line segments.
intersectionLL :: Line -> Line -> (Vec2, Angle, LLIntersection)
intersectionLL lineL lineR = (intersectionPoint, intersectionAngle, intersectionType)
  where
    intersectionType = case (intersectionInsideL, intersectionInsideR) of
        (True,  True)  -> IntersectionReal
        (True,  False) -> IntersectionVirtualInsideL
        (False, True)  -> IntersectionVirtualInsideR
        (False, False) -> IntersectionVirtual

    Line (Vec2 x1 y1) (Vec2 x2 y2) = lineL
    Line (Vec2 a1 b1) (Vec2 a2 b2) = lineR

    intersectionPoint = Vec2 (x1 + t * (x2-x1)) (y1 + t * (y2-y1))

    t =   ((x1-a1) * (b1-b2) - (y1-b1) * (a1-a2))
        / ---------------------------------------
          ((x1-x2) * (b1-b2) - (y1-y2) * (a1-a2))
    intersectionInsideL = t >= 0 && t <= 1

    u = - ((x1-x2) * (y1-b1) - (y1-y2) * (x1-a1))
        / ---------------------------------------
          ((x1-x2) * (b1-b2) - (y1-y2) * (a1-a2))
    intersectionInsideR = u >= 0 && u <= 1

    intersectionAngle = angleBetween lineL lineR

polygonEdges :: Polygon -> [Line]
polygonEdges (Polygon ps) = zipWith Line ps (tail (cycle ps))

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
            (_, _, IntersectionReal) -> True
            _other -> False)

    edges = polygonEdges poly

-- | Average of polygon vertices
polygonAverage :: Polygon -> Vec2
polygonAverage (Polygon corners)
  = let (num, total) = foldl' (\(!n, !vec) corner -> (n+1, addVec2 vec corner)) (0, Vec2 0 0) corners
    in mulVec2 (1/num) total

polygonCircumference :: Polygon -> Distance
polygonCircumference poly = foldl'
    (\(Distance acc) edge -> let Distance d = lineLength edge in Distance (acc + d))
    (Distance 0)
    (polygonEdges poly)

-- UNTESTED
--
-- http://mathworld.wolfram.com/PolygonArea.html
polygonArea :: Polygon -> Area
polygonArea (Polygon ps)
  = let determinants = zipWih (\(Vec2 x1 y1) (Vec2 x2 y2) -> x1*y2 - x2*y1) ps (tail (cycle ps))
    in Area (ans (sum determinants / 2))

-- UNTESTED
isConvex :: Polygon -> Bool
isConvex (Polygon ps)
  = let innerAnglesSines = zipWith3
            (\p q r -> sin (angleBetween (Line p q) (Line q r)))
            ps
            (tail (cycle ps))
            (tail (tail (cycle ps)))
    in all (>= 0) innerAnglesSines || all (<= 0) innerAnglesSines

-- | Cut a polygon in multiple pieces with a line.
cutPolygon :: Polygon -> Line -> [Polygon]
cutPolygon = error "TODO! See https://geidav.wordpress.com/2015/03/21/splitting-an-arbitrary-polygon-by-a-line/"
    -- This could be useful to shatter polygons into multiple pieces for a nice
    -- effect.

perpendicularBisector :: Line -> Line
perpendicularBisector line@(Line _start end) = rotateAround middle (Angle (pi/2)) line
  where
    middle = mulVec2 0.5 (end `addVec2` end)

deg :: Double -> Angle
deg degrees = rad (degrees / 360 * 2 * pi)

rad :: Double -> Angle
rad r = Angle (r `mod'` (2*pi))

-- | Line perpendicular to a given line through a point. The result starts
-- at that point and has unit length.
perpendicularLineThrough :: Vec2 -> Line -> Line
perpendicularLineThrough p line = angledLine p angle' (Distance 1)
  where
    Angle a = angleOfLine line
    angle' = Angle (a + pi/2)

-- | Optical reflection of a ray on a mirror. Note that the outgoing line has
-- reversed direction like light rays would.
reflection :: Line -> Line -> (Line, Vec2, Angle, LLIntersection)
reflection ray mirror = (lineReverse ray', iPoint, iAngle, iType)
  where
    (iPoint, iAngle, iType) = intersectionLL ray mirror
    mirrorAxis = perpendicularLineThrough iPoint mirror
    ray' = mirrorAlong mirrorAxis ray

-- | Shoot a billard ball inside a polygon along an initial line. Returns an
-- infinite list of collision points.
--
-- TODO: generalize to arbitrary geometries, not just polygons, to allow
-- polygon-in-polygon scenarios
billardProcess :: Polygon -> Line -> [Vec2]
billardProcess poly = go (const True)
  where
    edges = polygonEdges poly

    -- The predicate is used to exclude the line just mirrored off of, otherwise
    -- we get rays stuck in a single line due to numerical shenanigans. Note
    -- that this is a valid use case for equality of Double (contained in
    -- Line/Vec2). :-)
    go :: (Line -> Bool) -> Line -> [Vec2]
    go considerEdge ballVec@(Line ballStart _)
      = let reflectionRays :: [(Line, Line)]
            reflectionRays = do
                edge <- edges
                let (Line _ reflectionEnd, incidentPoint, _angle, ty) = reflection ballVec edge
                guard (case ty of
                    IntersectionReal           -> True
                    IntersectionVirtualInsideR -> True
                    IntersectionVirtualInsideL -> False
                    IntersectionVirtual        -> False )
                guard (incidentPoint `liesAheadOf` ballVec)
                guard (considerEdge edge)
                pure (edge, normalizeLine (Line incidentPoint reflectionEnd))

            (edgeReflectedOn, reflectionRay@(Line reflectionStart _)) = flip minimumBy reflectionRays
                (\(_, Line p _) (_, Line q _) ->
                    let Distance pDistance = lineLength (Line ballStart p)
                        Distance qDistance = lineLength (Line ballStart q)
                    in compare pDistance qDistance )
        in reflectionStart : go (/= edgeReflectedOn) reflectionRay

    liesAheadOf :: Vec2 -> Line -> Bool
    liesAheadOf p ray@(Line rayStart _)
      = let ray2 = Line rayStart p
            Angle angle = angleBetween ray ray2
        in cos angle > 0
