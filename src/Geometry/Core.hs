{-# LANGUAGE BangPatterns #-}

module Geometry.Core (
    -- * Primitives
    -- ** Vector spaces
      VectorSpace(..)

    -- ** 2D Vectors
    , Vec2(..)
    , dotProduct
    , norm
    , normSquare

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

    -- ** Polygons
    , Polygon(..)
    , normalizePolygon
    , PolygonError(..)
    , validatePolygon
    , pointInPolygon
    , countEdgeTraversals
    , polygonAverage
    , polygonCircumference
    , polygonArea
    , signedPolygonArea
    , polygonEdges
    , polygonAngles
    , isConvex
    , convexHull
    , PolygonOrientation(..)
    , polygonOrientation

    -- ** Angles
    , Angle(..)
    , deg
    , rad

    -- ** Safety newtypes
    , Distance(..)
    , Area(..)

    -- * Transformations
    , moveRad
    , Transformation(..)
    , identityTransformation
    , transformationProduct
    , inverse
    , Transform(..)
    , translateT
    , translate
    , rotateT
    , rotate
    , rotateAround
    , scaleT
    , scale
    , scaleXY
    , scaleAround
    , scaleAroundXY
    , mirrorT
    , mirrorAlongT
    , mirrorX
    , mirrorY
    , mirrorAlong

    -- * Processes
    , reflection
    , billardProcess

    -- * Useful stuff
    , vectorOf
    , det
    , direction
    , bugError
) where



import Control.Monad
import Data.Fixed
import Data.List
import Text.Printf

import Util



data Vec2 = Vec2 !Double !Double deriving (Eq, Ord, Show)

-- | Polygon, defined by its corners.
--
-- Many algorithms assume certain invariants about polygons, see
-- 'validatePolygon' for details.
newtype Polygon = Polygon [Vec2]

-- | List-rotate the polygon’s corners until the minimum is the first entry in
-- the corner list.
normalizePolygon :: Polygon -> Polygon
normalizePolygon (Polygon corners) = Polygon (rotateUntil (== minimum corners) corners)

instance Eq Polygon where
    p1 == p2
      = let Polygon p1Edges@(edge1:_) = p1
            Polygon p2Edges = p2
            p2Edges' = rotateUntil (== edge1) p2Edges
        in p1Edges == p2Edges'

instance Ord Polygon where
    compare p1 p2
      = let Polygon p1Edges = normalizePolygon p1
            Polygon p2Edges = normalizePolygon p2
        in compare p1Edges p2Edges

instance Show Polygon where
    show poly = let Polygon corners = normalizePolygon poly
                in "Polygon " ++ show corners

-- | Line, defined by beginning and end.
data Line = Line Vec2 Vec2 deriving (Eq, Ord, Show)



-- | Affine transformation,
--
-- > transformation a b c
-- >                d e f
-- > ==>
-- > / a b \ + / c \
-- > \ d e /   \ f /
--
-- Transformations can be chained using '<>', but in general it’s often more
-- convenient to use the predefined functions such as 'rotateT with '.' as composition.
data Transformation = Transformation Double Double Double
                                     Double Double Double
                                     deriving (Eq, Ord, Show)

identityTransformation :: Transformation
identityTransformation = Transformation
    1 0 0
    0 1 0

transformationProduct :: Transformation -> Transformation -> Transformation
transformationProduct (Transformation a1 b1 c1
                                      d1 e1 f1)
                      (Transformation a2 b2 c2
                                      d2 e2 f2)
                    =  Transformation (a1*a2 + b1*d2) (a1*b2 + b1*e2) (a1*c2 + b1*f2 + c1)
                                      (d1*a2 + e1*d2) (d1*b2 + e1*e2) (d1*c2 + e1*f2 + f1)

inverse :: Transformation -> Transformation
inverse (Transformation a b c
                        d e f)
    = let x = 1 / (a*e - b*d)
      in Transformation (x*e) (x*(-b)) (x*(-e*c + b*f))
                        (x*(-d)) (x*a) (x*(d*c - a*f))

instance Semigroup Transformation where
    (<>) = transformationProduct

instance Monoid Transformation where
    mempty = identityTransformation

class Transform geo where
    transform :: Transformation -> geo -> geo

instance Transform Vec2 where
    transform (Transformation a b c
                              d e f)
              (Vec2 x y)
            = Vec2 (a*x + b*y + c) (d*x + e*y + f)

instance Transform Line where
    transform t (Line start end) = Line (transform t start) (transform t end)

instance Transform Polygon where
    transform t (Polygon ps) = Polygon (map (transform t) ps)

instance Transform Transformation where
    transform = transformationProduct
    -- ^ Right argument will be applied first, so that
    --
    -- > rotate `transform` translate
    --
    -- will translate before rotating.

instance Transform a => Transform [a] where
    transform t = map (transform t)

translateT :: Vec2 -> Transformation
translateT (Vec2 dx dy) = Transformation
    1 0 dx
    0 1 dy

translate :: Transform geo => Vec2 -> geo -> geo
translate v = transform (translateT v)

-- deprecated
move :: Transform geo => Vec2 -> geo -> geo
move = translate

-- deprecated? at least rename
moveRad :: Transform geo => Angle -> Distance -> geo -> geo
moveRad (Angle a) (Distance d) = move (Vec2 (d * cos a) (d * sin a))

rotateT :: Angle -> Transformation
rotateT (Angle a) = Transformation
    (cos a) (-sin a) 0
    (sin a) ( cos a) 0

rotate :: Transform geo => Angle -> geo -> geo
rotate angle = transform (rotateT angle)

rotateAround :: Transform geo => Vec2 -> Angle -> geo -> geo
rotateAround pivot angle = transform (translateT pivot <> rotateT angle <> inverse (translateT pivot))

scaleT :: Double -> Double -> Transformation
scaleT x y = Transformation
    x 0 0
    0 y 0

scale :: Transform geo => Double -> geo -> geo
scale factor = transform (scaleT factor factor)

scaleAround :: Transform geo => Vec2 -> Double -> geo -> geo
scaleAround pivot factor = transform (translateT pivot <> scaleT factor factor <> inverse (translateT pivot))

scaleXY :: Transform geo => Double -> Double -> geo -> geo
scaleXY x y = transform (scaleT x y)

scaleAroundXY :: Transform geo => Vec2 -> Double -> Double -> geo -> geo
scaleAroundXY pivot x y = transform (translateT pivot <> scaleT x y <> inverse (translateT pivot))

mirrorT :: Angle -> Transformation
mirrorT angle = rotateT angle <> mirrorVertically <> inverse (rotateT angle)
  where
    mirrorVertically = Transformation
        1    0 0
        0 (-1) 0

mirrorAlongT :: Line -> Transformation
mirrorAlongT line@(Line p _) = translateT p <> mirrorT (angleOfLine line) <> inverse (translateT p)

mirrorX, mirrorY :: Transform geo => geo -> geo
mirrorX = transform (mirrorT (deg 90))
mirrorY = transform (mirrorT (deg 0))

mirrorAlong :: Transform geo => Line -> geo -> geo
mirrorAlong line = transform (mirrorAlongT line)


-- | A generic vector space. Not only classic vectors like 'Vec2' form a vector
-- space, but also concepts like 'Angle's or 'Distance's – anything that can be
-- added, inverted, and multiplied with a scalar.
--
-- Vector space laws:
--
--     (1) Associativity of addition: @a +. (b +. c) = (a +. b) +. c@
--     (2) Neutral ('zero'): @a +. 'zero' = a = 'zero' +. a@
--     (3) Inverse ('negateV'): @a +. 'negateV' a = 'zero' = 'negateV' a +. a@. '(-.)' is a shorthand for the inverse: @a -. b = a +. negate b@.
--     (4) Commutativity of addition: @a +. b = b +. a@
--     (5) Distributivity of scalar multiplication 1: @a *. (b +. c) = a *. b +. a *. c@
--     (6) Distributivity of scalar multiplication 2: @(a + b) *. c = a *. c +. b *. c@
--     (7) Compatibility of scalar multiplication: @(a * b) *. c = a *. (b *. c)@
--     (8) Scalar identity: @1 *. a = a@
class VectorSpace v where
    {-# MINIMAL (+.), (*.), ((-.) | negateV), zero #-}
    -- | Vector addition
    (+.) :: v -> v -> v

    -- | Vector subtraction
    (-.) :: v -> v -> v
    a -. b = a +. negateV b

    -- | Multiplication with a scalar
    (*.) :: Double -> v -> v

    -- | Inverse element
    negateV :: v -> v
    negateV a = zero -. a

    -- | Neutral
    zero :: v

infixl 6 +., -.
infixl 7 *.

instance VectorSpace Vec2 where
    Vec2 x1 y1 +. Vec2 x2 y2 = Vec2 (x1+x2) (y1+y2)
    a *. Vec2 x y = Vec2 (a*x) (a*y)
    negateV (Vec2 x y) = Vec2 (-x) (-y)
    zero = Vec2 0 0

dotProduct :: Vec2 -> Vec2 -> Double
dotProduct (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

-- | Euclidean norm.
norm :: Vec2 -> Distance
norm = Distance . sqrt . normSquare

-- | Squared Euclidean norm. Does not require a square root, and is thus
-- suitable for sorting points by distance without excluding certain kinds of
-- numbers such as rationals.
normSquare :: Vec2 -> Double
normSquare v = dotProduct v v

-- | Newtype safety wrapper.
newtype Angle = Angle Double deriving (Eq, Ord)

instance Show Angle where
    show (Angle a) = printf "deg %2.8f" (a / pi * 180)

instance VectorSpace Angle where
    Angle a +. Angle b = rad (a + b)
    Angle a -. Angle b = rad (a - b)
    a *. Angle b = rad (a * b)
    negateV (Angle a) = rad (-a)
    zero = Angle 0

-- | Degrees-based 'Angle' smart constructor.
deg :: Double -> Angle
deg degrees = rad (degrees / 360 * 2 * pi)

-- | Radians-based 'Angle' smart constructor.
rad :: Double -> Angle
rad r = Angle (r `mod'` (2*pi))

-- | Newtype safety wrapper.
newtype Distance = Distance Double deriving (Eq, Ord, Show)

instance VectorSpace Distance where
    Distance a +. Distance b = Distance (a + b)
    Distance a -. Distance b = Distance (a - b)
    a *. Distance b = Distance (a * b)
    negateV (Distance a) = Distance (-a)
    zero = Distance 0

-- | Newtype safety wrapper.
newtype Area = Area Double deriving (Eq, Ord, Show)


-- | Directional vector of a line, i.e. the vector pointing from start to end.
-- The norm of the vector is the length of the line. Use 'normalizeLine' to make
-- it unit length.
vectorOf :: Line -> Vec2
vectorOf (Line start end) = end -. start

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
    end = rotateAround start angle (start +. Vec2 len 0)

lineLength :: Line -> Distance
lineLength = norm . vectorOf

-- | Resize a line, keeping the starting point.
resizeLine :: (Distance -> Distance) -> Line -> Line
resizeLine f line@(Line start _end)
  = let v = vectorOf line
        len@(Distance d) = norm v
        Distance d' = f len
        v' = (d'/d) *. v
        end' = start +. v'
    in Line start end'

-- | Resize a line, keeping the middle point.
resizeLineSymmetric :: (Distance -> Distance) -> Line -> Line
resizeLineSymmetric f line@(Line start end) = (centerLine . resizeLine f . move delta) line
  where
    middle = 0.5 *. (start +. end)
    delta = middle -. start

-- | Move the line so that its center is where the start used to be.
--
-- Useful for painting lines going through a point symmetrically.
centerLine :: Line -> Line
centerLine line@(Line start end) = move delta line
  where
    middle = 0.5 *. (start +. end)
    delta = start -. middle

-- | Move the end point of the line so that it has length 1.
normalizeLine :: Line -> Line
normalizeLine = resizeLine (const (Distance 1))

-- | Direction vector of a line.
direction :: Line -> Vec2
direction = vectorOf . normalizeLine

-- | Switch defining points of a line.
lineReverse :: Line -> Line
lineReverse (Line start end) = Line end start

bugError :: String -> a
bugError msg = errorWithoutStackTrace (msg ++ "\nThis should never happen! Please report it as a bug.")

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
        in Vec2 ( (det v1 v2 * (x3-x4) - (x1-x2) * det v3 v4) / denominator )
                ( (det v1 v2 * (y3-y4) - (y1-y2) * det v3 v4) / denominator )

    t = det (v1 -. v3) (v3 -. v4) / det (v1 -. v2) (v3 -. v4)
    intersectionInsideL = t >= 0 && t <= 1

    u = - det (v1 -. v2) (v1 -. v3) / det (v1 -. v2) (v3 -. v4)
    intersectionInsideR = u >= 0 && u <= 1

polygonEdges :: Polygon -> [Line]
polygonEdges (Polygon ps) = zipWith Line ps (tail (cycle ps))

polygonAngles :: Polygon -> [Angle]
polygonAngles polygon@(Polygon corners)
  = let orient = case polygonOrientation polygon of
            PolygonNegative -> flip
            PolygonPositive -> id
        angle p x q = orient angleBetween (Line x q) (Line x p)
        _ : corners1 : corners2 : _ = iterate tail (cycle corners)
    in zipWith3 angle corners corners1 corners2

-- | The smallest convex polygon that contains all points.
--
-- The result is oriented in mathematically positive direction. (Note that Cairo
-- uses a left-handed coordinate system, so mathematically positive is drawn as
-- clockwise.)
convexHull :: [Vec2] -> Polygon
-- Andrew’s algorithm
convexHull points
  = let pointsSorted = sort points
        angleSign a b c = signum (det (b -. a) (c -. b))
        go :: (Double -> Double -> Bool) -> [Vec2] -> [Vec2] -> [Vec2]
        go cmp [] (p:ps) = go cmp [p] ps
        go cmp [s] (p:ps) = go cmp [p,s] ps
        go cmp (s:t:ack) (p:ps)
            | angleSign t s p `cmp` 0 = go cmp (p:s:t:ack) ps
            | otherwise = go cmp (t:ack) (p:ps)
        go _ stack [] = stack

    in Polygon (drop 1 (go (<=) [] pointsSorted) ++ drop 1 (reverse (go (>=) [] pointsSorted)))

-- | Orientation of a polygon
data PolygonOrientation = PolygonPositive | PolygonNegative
    deriving (Eq, Ord, Show)

polygonOrientation :: Polygon -> PolygonOrientation
polygonOrientation polygon
    | signedPolygonArea polygon >= Area 0 = PolygonPositive
    | otherwise                           = PolygonNegative

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
    -- The test ray comes from outside the polygon, and ends at the point to be
    -- tested.
    --
    -- This ray is numerically sensitive, because exactly crossing a corner of
    -- the polygon counts as two traversals (with each adjacent edge), when it
    -- should only be one.  For this reason, we subtract 1 from the y coordinate
    -- as well to get a bit of an odd angle, greatly reducing the chance of
    -- exactly hitting a corner on the way.
    testRay = Line (Vec2 (leftmostPolyX - 1) (pointY - 1)) p
      where
        leftmostPolyX = minimum (edges >>= \(Line (Vec2 x1 _) (Vec2 x2 _)) -> [x1,x2])
        Vec2 _ pointY = p

    intersections = filter (\edge ->
        case intersectionLL testRay edge of
            (_, IntersectionReal) -> True
            _other -> False)
        edges

pointInPolygon :: Vec2 -> Polygon -> Bool
pointInPolygon p poly = odd (countEdgeTraversals p (polygonEdges poly))

data PolygonError
    = NotEnoughCorners Int
    | IdenticalPoints [Vec2]
    | SelfIntersections [(Line, Line)]
    deriving (Eq, Ord, Show)

-- | Check whether the polygon satisfies the invariants assumed by many
-- algorithms,
--
--   * At least three corners
--   * No identical points
--   * No self-intersections
--
-- Returns the provided polygon on success.
validatePolygon :: Polygon -> Either PolygonError Polygon
validatePolygon = \polygon -> do
    threeCorners polygon
    noIdenticalPoints polygon
    noSelfIntersections polygon
    pure polygon
  where
    threeCorners (Polygon ps) = case ps of
        (_1:_2:_3:_) -> pure ()
        _other       -> Left (NotEnoughCorners (length ps))

    noIdenticalPoints (Polygon corners) = case nub' corners of
        uniques | uniques == corners -> pure ()
                | otherwise -> Left (IdenticalPoints (corners \\ uniques))

    noSelfIntersections polygon = case selfIntersectionPairs polygon of
        [] -> pure ()
        intersections -> Left (SelfIntersections intersections)

    selfIntersectionPairs :: Polygon -> [(Line, Line)]
    selfIntersectionPairs poly
      = [ (edge1, edge2) | _:edge1:_:restEdges <- tails (polygonEdges poly)
                         , edge2 <- restEdges
                         -- Skip neighbouring edge because neighbours always intersect
                         -- , let Line e11 _e12 = edge1
                         -- , let Line _e21 e22 = edge2
                         -- -- , e12 /= e21
                         -- , e11 /= e22
                         , (_, IntersectionReal) <- [intersectionLL edge1 edge2]
                         ]

-- | Average of polygon vertices
polygonAverage :: Polygon -> Vec2
polygonAverage (Polygon corners)
  = let (num, total) = foldl' (\(!n, !vec) corner -> (n+1, vec +. corner)) (0, Vec2 0 0) corners
    in (1/num) *. total

polygonCircumference :: Polygon -> Distance
polygonCircumference poly = foldl'
    (\(Distance acc) edge -> let Distance d = lineLength edge in Distance (acc + d))
    (Distance 0)
    (polygonEdges poly)

-- | Determinant of the matrix
--
-- > / x1 x2 \
-- > \ y1 y2 /
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

signedPolygonArea :: Polygon -> Area
signedPolygonArea (Polygon ps)
  = let determinants = zipWith det ps (tail (cycle ps))
    in Area (sum determinants / 2)

isConvex :: Polygon -> Bool
isConvex (Polygon ps)
    -- The idea is that a polygon is convex iff all internal angles are in the
    -- same direction. The direction of an angle defined by two vectors shares
    -- its sign with the signed area spanned by those vectors, and the latter is
    -- easy to calculate via a determinant.
  = let angleDotProducts = zipWith3
            (\p q r ->
                let lineBeforeAngle = Line p q
                    lineAfterAngle  = Line q r
                in det (vectorOf lineBeforeAngle) (vectorOf lineAfterAngle) )
            ps
            (tail (cycle ps))
            (tail (tail (cycle ps)))

        allSameSign :: [Double] -> Bool
        -- NB: head is safe here, since all short-circuits for empty xs
        allSameSign xs = all (\p -> signum p == signum (head xs)) xs
    in allSameSign angleDotProducts

-- | The result has the same length as the input, point in its center, and
-- points to the left (90° turned CCW) relative to the input.
perpendicularBisector :: Line -> Line
perpendicularBisector line@(Line start end) = perpendicularLineThrough middle line
  where
    middle = 0.5 *. (start +. end)

-- | Line perpendicular to a given line through a point.
--
-- The result has the same length as the input, point in its center, and points
-- to the left (90° turned CCW) relative to the input.
perpendicularLineThrough :: Vec2 -> Line -> Line
perpendicularLineThrough p line@(Line start _) = centerLine line'
  where
    -- Move line so it starts at the origin
    Line start0 end0 = move (negateV start) line
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
    liesAheadOf point (Line rayStart rayEnd)
      = dotProduct (point -. rayStart) (rayEnd -. rayStart) > 0

    distanceFrom :: Vec2 -> Vec2 -> Vec2 -> Ordering
    distanceFrom start p q
      = let Distance pDistance = lineLength (Line start p)
            Distance qDistance = lineLength (Line start q)
        in compare pDistance qDistance
