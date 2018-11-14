{-# LANGUAGE BangPatterns #-}

module Geometry (
    -- * Primitives
    -- ** Vectors
    Vec2(..)
    , addVec2
    , negateVec2
    , subtractVec2
    , mulVec2
    , dotProduct
    , norm

    -- ** Lines
    , Line(..)
    , angleOfLine
    , angleBetween
    , angledLine
    , lineLength
    , resizeLine
    , resizeLineSymmetric
    , centerLine
    , normalizeLine
    , lineReverse
    , perpendicularBisector
    , perpendicularLineThrough
    , LLIntersection(..)
    , intersectionLL
    , cutLine
    , CutLine(..)

    -- ** Polygons
    , Polygon(..)
    , pointInPolygon
    , countEdgeTraversals
    , polygonAverage
    , polygonCircumference
    , polygonArea
    , isConvex
    , polygonEdges
    , cutPolygon

    -- ** Angles
    , Angle(..)
    , deg
    , rad

    -- ** Safety newtypes
    , Distance(..)
    , Area(..)

    -- ** Convenience classes
    , Move(..)
    , moveRad
    , Rotate(..)
    , Mirror(..)

    -- * Processes
    , reflection
    , billardProcess
) where



import Control.Monad
import Data.Fixed
import Data.List
import Text.Printf



data Vec2 = Vec2 !Double !Double deriving (Eq, Ord, Show)

-- | Polygon, defined by its corners.
newtype Polygon = Polygon [Vec2] deriving (Eq, Ord, Show)

-- | Line, defined by beginning and end.
data Line = Line Vec2 Vec2 deriving (Eq, Ord, Show)

-- | Newtype safety wrapper.
newtype Angle = Angle Double deriving (Eq, Ord)

-- | Newtype safety wrapper.
newtype Distance = Distance Double deriving (Eq, Ord, Show)

-- | Newtype safety wrapper.
newtype Area = Area Double deriving (Eq, Ord, Show)

instance Show Angle where
    show (Angle a) = printf "deg %2.8f" (a / pi * 180)

class Move geo where
    move :: Vec2 -> geo -> geo

moveRad :: Move geo => Angle -> Distance -> geo -> geo
moveRad (Angle a) (Distance d) = move (Vec2 (d * cos a) (d * sin a))

instance Move Vec2 where
    move = addVec2

instance Move Polygon where
    move offset (Polygon points) = Polygon (map (move offset) points)

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

class Mirror geo where
    mirrorAlong :: Line -> geo -> geo

instance Mirror Vec2 where
    mirrorAlong mirror p
      = let perpendicular = perpendicularLineThrough p mirror
            (foot, _ty) = intersectionLL mirror perpendicular
        in foot `addVec2` foot `subtractVec2` p

instance Mirror Line where
    mirrorAlong mirror (Line start end) = Line (mirrorAlong mirror start)
                                               (mirrorAlong mirror end)

instance Mirror Polygon where
    mirrorAlong mirror (Polygon ps) = Polygon (map (mirrorAlong mirror) ps)

infixl 6 `addVec2`, `subtractVec2`
infixl 7 `mulVec2`, `dotProduct`

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

-- | Euclidean norm.
norm :: Vec2 -> Distance
norm v = Distance (sqrt (dotProduct v v))

-- | Degrees-based 'Angle' smart constructor.
deg :: Double -> Angle
deg degrees = Angle (degrees / 360 * 2 * pi)

-- | Radians-based 'Angle' smart constructor.
rad :: Double -> Angle
rad r = Angle (r `mod'` (2*pi))

-- | Angle of a single line, relative to the x axis.
angleOfLine :: Line -> Angle
angleOfLine (Line (Vec2 x1 y1) (Vec2 x2 y2)) = rad (atan2 (y2-y1) (x2-x1))

angleBetween :: Line -> Line -> Angle
angleBetween line1 line2
  = let Angle a1 = angleOfLine line1
        Angle a2 = angleOfLine line2
    in rad (a2 - a1)

angledLine :: Vec2 -> Angle -> Distance -> Line
angledLine start angle (Distance len) = Line start end
  where
    end = rotateAround start angle (start `addVec2` Vec2 len 0)

lineLength :: Line -> Distance
lineLength (Line start end) = norm (end `subtractVec2` start)

-- | Resize a line, keeping the starting point.
resizeLine :: Line -> (Distance -> Distance) -> Line
resizeLine line@(Line start _end) f
  = angledLine start (angleOfLine line) (f (lineLength line))

-- | Resize a line, extending in both directions.
resizeLineSymmetric :: Line -> (Distance -> Distance) -> Line
resizeLineSymmetric line f = centerLine (resizeLine line f )

-- | Move the line so that its center is where the start used to be.
--
-- Useful for painting lines going through a point symmetrically.
centerLine :: Line -> Line
centerLine line@(Line start end) = move delta line
  where
    middle = mulVec2 0.5 (start `addVec2` end)
    delta = start `subtractVec2` middle

-- | Move the end point of the line so that it has length 1.
normalizeLine :: Line -> Line
normalizeLine line = resizeLine line (const (Distance 1))

-- | Switch defining points of a line
lineReverse :: Line -> Line
lineReverse (Line start end) = Line end start

data LLIntersection
    = IntersectionReal
        -- ^ Two lines intersect fully.

    | IntersectionVirtualInsideL
        -- ^ The intersection is in the left argument (of 'intersectionLL')
        -- only, and only on the infinite continuation of the right argument.

    | IntersectionVirtualInsideR
        -- ^ dito, but the other way round.

    | IntersectionVirtual
        -- ^ The intersection lies in the infinite continuations of both lines.
    deriving (Eq, Ord, Show)

-- | Calculate the intersection of two lines.
--
-- Returns the point of the intersection, and whether it is inside both, one, or
-- none of the provided finite line segments.
intersectionLL :: Line -> Line -> (Vec2, LLIntersection)
intersectionLL lineL lineR = (intersectionPoint, intersectionType)
  where
    intersectionType = case (intersectionInsideL, intersectionInsideR) of
        (True,  True)  -> IntersectionReal
        (True,  False) -> IntersectionVirtualInsideL
        (False, True)  -> IntersectionVirtualInsideR
        (False, False) -> IntersectionVirtual

    -- Calculation copied straight off of Wikipedia, then converted Latex to
    -- Haskell using bulk editing.
    --
    -- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection

    Line v1@(Vec2 x1 y1) v2@(Vec2 x2 y2) = lineL
    Line v3@(Vec2 x3 y3) v4@(Vec2 x4 y4) = lineR

    intersectionPoint
      = let denominator = (x1-x2) * (y3-y4) - (y1-y2) * (x3-x4)
        in Vec2 ( ((det v1 v2) * (x3-x4) - (x1-x2) * (det v3 v4)) / denominator )
                ( ((det v1 v2) * (y3-y4) - (y1-y2) * (det v3 v4)) / denominator )

    t = det (v1 -. v3) (v3 -. v4) / det (v1 -. v2) (v3 -. v4)
    intersectionInsideL = t >= 0 && t <= 1

    u = - det (v1 -. v2) (v1 -. v3) / det (v1 -. v2) (v3 -. v4)
    intersectionInsideR = u >= 0 && u <= 1

    (-.) = subtractVec2

polygonEdges :: Polygon -> [Line]
polygonEdges (Polygon ps) = zipWith Line ps (tail (cycle ps))

-- | Ray-casting algorithm. Counts how many times a ray coming from infinity
-- intersects the edges of an object.
--
-- The most basic use case is 'pointInPolygon', but it can also be used to find
-- out whether something is inside more complicated objects, such as nested
-- polygons (e.g. polygons with holes).
countEdgeTraversals
    :: Vec2   -- ^ Point to check
    -> [Line] -- ^ Geometry
    -> Int    -- ^ Number of edges crossed
countEdgeTraversals p edges = length intersections
  where
    -- The test ray comes from outside the polygon from the left, and ends at
    -- the point to be tested.
    testRay = Line (Vec2 (leftmostPolyX - 1) pointY) p
      where
        leftmostPolyX = minimum (edges >>= \(Line (Vec2 x1 _) (Vec2 x2 _)) -> [x1,x2])
        Vec2 _ pointY = p

    intersections = flip filter edges (\edge ->
        case intersectionLL testRay edge of
            (_, IntersectionReal) -> True
            _other -> False)

pointInPolygon :: Vec2 -> Polygon -> Bool
pointInPolygon p poly = odd (countEdgeTraversals p (polygonEdges poly))

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

-- | Determinant of the matrix
--
-- / x1 x2 \
-- \ y1 y2 /
--
-- This is useful to calculate the (signed) area of the parallelogram spanned by
-- two vectors.
det :: Vec2 -> Vec2 -> Double
det (Vec2 x1 y1) (Vec2 x2 y2) = x1*y2 - y1*x2

-- UNTESTED
--
-- http://mathworld.wolfram.com/PolygonArea.html
polygonArea :: Polygon -> Area
polygonArea (Polygon ps)
  = let determinants = zipWith det ps (tail (cycle ps))
    in Area (abs (sum determinants / 2))

-- UNTESTED
isConvex :: Polygon -> Bool
isConvex (Polygon ps)
    -- The idea is that a polygon is convex iff all internal angles are in the
    -- same direction. The direction of an angle defined by two vectors shares
    -- its sign with the signed area spanned by those vectors, and the latter is
    -- easy to calculate via a determinant.
  = let angleDotProducts = zipWith3
            (\p q r ->
                let vectorOf (Line start end) = end `subtractVec2` start
                    lineBeforeAngle = Line p q
                    lineAfterAngle  = Line q r
                in det (vectorOf lineBeforeAngle) (vectorOf lineAfterAngle) )
            ps
            (tail (cycle ps))
            (tail (tail (cycle ps)))

        allSameSign :: [Double] -> Bool
        -- NB: head is safe here, since all short-circuits for empty xs
        allSameSign xs = all (\p -> signum p == signum (head xs)) xs
    in allSameSign angleDotProducts

-- | Cut a polygon in multiple pieces with a line.
--
-- Algorithm inspired by https://geidav.wordpress.com/2015/03/21/splitting-an-arbitrary-polygon-by-a-line/
cutPolygon :: Line -> Polygon -> [Polygon]
cutPolygon scissors polygon
  = let extendedPolygon :: [CutLine]
        extendedPolygon = map (cutLine scissors) (polygonEdges polygon)

        isCut Cut{} = True
        isCut NoCut{} = False

        rotateToFirstSource :: [CutLine] -> [CutLine]
        rotateToFirstSource cuts = zipWith const
            (dropWhile (not . isCut) (cycle cuts))
            (undefined : cuts)

        -- Cut: Finalize current polygon, start a new one
        walk (Polygon corners) (Cut p q r : rest)
          = Polygon (reverse (q:p:corners)) : walk (Polygon []) (NoCut q r : rest)

        -- No cut: extend current polygon
        walk (Polygon corners) (NoCut p _ : rest)
          = walk (Polygon (p:corners)) rest

        -- Out of potential cuts, terminate
        walk _ [] = []

    in walk (Polygon []) (rotateToFirstSource extendedPolygon)

data CutLine
    = NoCut Vec2 Vec2
        -- ^ (start, end). No cut has occurred, i.e. the cutting line did not
        -- intersect with the object.
    | Cut Vec2 Vec2 Vec2
        -- ^ (start, cut, end). The input was divided in two lines.
    deriving (Eq, Ord, Show)

-- | Cut a finite piece of paper in one or two parts with an infinite line
cutLine :: Line -> Line -> CutLine
cutLine scissors paper = case intersectionLL scissors paper of
    (p, IntersectionReal)           -> cut p
    (p, IntersectionVirtualInsideR) -> cut p
    (_, IntersectionVirtualInsideL) -> noCut
    (_, IntersectionVirtual)        -> noCut
  where
    Line paperStart paperEnd = paper
    cut p = Cut paperStart p paperEnd
    noCut = NoCut paperStart paperEnd

-- | The result has the same length as the input, point in its center, and
-- points to the left (90° turned CCW) relative to the input.
perpendicularBisector :: Line -> Line
perpendicularBisector line@(Line start end) = perpendicularLineThrough middle line
  where
    middle = mulVec2 0.5 (start `addVec2` end)

-- | Line perpendicular to a given line through a point.
--
-- The result has the same length as the input, point in its center, and points
-- to the left (90° turned CCW) relative to the input.
perpendicularLineThrough :: Vec2 -> Line -> Line
perpendicularLineThrough p line@(Line start _) = centerLine line'
  where
    -- Move line to it starts at the origin
    Line start0 end0 = move (negateVec2 start) line
    -- Rotate end point 90° CCW
    end0' = let Vec2 x y  = end0
            in Vec2 (-y) x
    -- Construct rotated line
    lineAt0' = Line start0 end0'
    -- Move line back so it goes through the point
    line' = move p lineAt0'

-- | Optical reflection of a ray on a mirror. Note that the outgoing line has
-- reversed direction like light rays would. The second result element is the
-- point of intersection with the mirror, which is not necessarily on the line,
-- and thus returned separately.
reflection
    :: Line -- ^ Light ray
    -> Line -- ^ Mirror
    -> (Line, Vec2, LLIntersection)
            -- ^ Reflected ray; point of incidence; type of intersection of the
            -- ray with the mirror. The reflected ray is symmetric with respect
            -- to the incoming ray (in terms of length, distance from mirror,
            -- etc.), but has reversed direction (like real light).
reflection ray mirror = (lineReverse ray', iPoint, iType)
  where
    (iPoint, iType) = intersectionLL ray mirror
    mirrorAxis = perpendicularLineThrough iPoint mirror
    ray' = mirrorAlong mirrorAxis ray

-- | Shoot a billard ball, and record its trajectory as it is reflected off the
-- edges of a provided geometry.
billardProcess
    :: [Line] -- ^ Geometry; typically involves the edges of a bounding polygon.
    -> Line   -- ^ Initial velocity vector of the ball. Only start and direction,
              --   not length, are relevant for the algorithm.
    -> [Vec2] -- ^ List of collision points. Finite iff the ball escapes the
              --   geometry.
billardProcess edges = go (const True)
  where
    -- The predicate is used to exclude the line just mirrored off of, otherwise
    -- we get rays stuck in a single line due to numerical shenanigans. Note
    -- that this is a valid use case for equality of Double (contained in
    -- Line/Vec2). :-)
    go :: (Line -> Bool) -> Line -> [Vec2]
    go considerEdge ballVec@(Line ballStart _)
      = let reflectionRays :: [(Line, Line)]
            reflectionRays = do
                edge <- edges
                let (Line _ reflectionEnd, incidentPoint, ty) = reflection ballVec edge
                guard (case ty of
                    IntersectionReal           -> True
                    IntersectionVirtualInsideR -> True
                    IntersectionVirtualInsideL -> False
                    IntersectionVirtual        -> False )
                guard (incidentPoint `liesAheadOf` ballVec)
                guard (considerEdge edge)
                pure (edge, Line incidentPoint reflectionEnd)

        in case reflectionRays of
            [] -> let Line _ end = ballVec in [end]
            _  ->
                let (edgeReflectedOn, reflectionRay@(Line reflectionStart _))
                      = minimumBy
                          (\(_, Line p _) (_, Line q _) -> distanceFrom ballStart p q)
                          reflectionRays
                in reflectionStart : go (/= edgeReflectedOn) reflectionRay

    liesAheadOf :: Vec2 -> Line -> Bool
    liesAheadOf p ray@(Line rayStart _)
      = let ray2 = Line rayStart p
            Angle angle = angleBetween ray ray2
        in cos angle > 0

    distanceFrom :: Vec2 -> Vec2 -> Vec2 -> Ordering
    distanceFrom start p q
      = let Distance pDistance = lineLength (Line start p)
            Distance qDistance = lineLength (Line start q)
        in compare pDistance qDistance
