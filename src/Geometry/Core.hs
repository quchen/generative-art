{-# LANGUAGE BangPatterns #-}

module Geometry.Core (
    -- * Primitives
    -- ** Vector spaces
      VectorSpace(..)
    , vsum

    -- ** 2D Vectors
    , Vec2(..)
    , dotProduct
    , norm
    , normSquare
    , polar

    -- ** Lines
    , Line(..)
    , angleOfLine
    , angleBetween
    , angledLine
    , lineLength
    , moveAlongLine
    , resizeLine
    , resizeLineSymmetric
    , centerLine
    , normalizeLine
    , lineReverse
    , perpendicularBisector
    , perpendicularLineThrough
    , distanceFromLine
    , LLIntersection(..)
    , intersectionLL
    , intersectionPoint
    , reflection

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
    , Angle
    , deg
    , getDeg
    , rad
    , getRad
    , normalizeAngle

    -- * Transformations
    , Transformation(..)
    , identityTransformation
    , transformationProduct
    , inverse
    , Transform(..)
    , translate
    , rotate
    , rotateAround
    , scale
    , scale'
    , scaleAround
    , scaleAround'
    , mirrorAlong
    , mirrorXCoords
    , mirrorYCoords

    -- * Bounding Box
    , HasBoundingBox(..)
    , BoundingBox(..)
    , overlappingBoundingBoxes
    , transformBoundingBox
    , ScalingBehavior(..)
    , boundingBoxPolygon
    , insideBoundingBox
    , boundingBoxCenter
    , boundingBoxSize

    -- * Useful stuff
    , vectorOf
    , det
    , direction
    , bugError
) where



import Algebra.VectorSpace
import Data.Fixed
import Data.List
import Text.Printf
import Control.DeepSeq
import qualified System.Random.MWC as MWC

import Util



data Vec2 = Vec2 !Double !Double deriving (Eq, Ord, Show)

instance NFData Vec2 where rnf _ = ()

instance MWC.UniformRange Vec2 where
    uniformRM (Vec2 xMin yMin, Vec2 xMax yMax) gen =
        Vec2 <$> MWC.uniformRM (xMin, xMax) gen <*> MWC.uniformRM (yMin, yMax) gen

-- | Polygon, defined by its corners.
--
-- Many algorithms assume certain invariants about polygons, see
-- 'validatePolygon' for details.
newtype Polygon = Polygon [Vec2]

instance NFData Polygon where rnf (Polygon xs) = rnf xs

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

-- | Rotate a list until the predicate holds. If it never holds, return the
-- input list.
rotateUntil :: (a -> Bool) -> [a] -> [a]
rotateUntil p xs = zipWith
    (flip const)
    xs
    (dropWhile (not . p) (cycle xs))

instance Ord Polygon where
    compare p1 p2
      = let Polygon p1Edges = normalizePolygon p1
            Polygon p2Edges = normalizePolygon p2
        in compare p1Edges p2Edges

instance Show Polygon where
    show poly = let Polygon corners = normalizePolygon poly
                in "Polygon " ++ show corners

-- | Line, defined by beginning and end.
data Line = Line !Vec2 !Vec2 deriving (Eq, Ord, Show)

instance NFData Line where rnf _ = ()

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
data Transformation = Transformation !Double !Double !Double
                                     !Double !Double !Double
                                     deriving (Eq, Ord, Show)

instance NFData Transformation where rnf _ = ()

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

-- | The order transformations are applied in function order:
--
-- @
-- transform (scale a b <> translate p)
-- ==
-- transform (scale a b) . translate p
-- @
--
-- In other words, this first translates its argument, and then scales.
-- Note that Cairo does its Canvas transformations just the other way round, since
-- in Cairo you do not move the geometry, but the coordinate system. If you wrap a
-- transformation in 'inverse', you get the Cairo behavior.
instance Semigroup Transformation where
    (<>) = transformationProduct

instance Monoid Transformation where
    mempty = identityTransformation

-- | Transform geometry using an affine transformation.
--
-- This allows for a multitude of common transformations, such as translation
-- ('translate'), rotation ('rotate') or scaling ('scale').
--
-- Simple transformations can be combined to yield more complex operations, such as
-- rotating around a point, which can be achieved by moving the center of rotation
-- to the origin, rotating, and then rotating back:
--
-- @
-- 'rotateAround' pivot angle = 'translate' pivot <> 'rotate' angle <> 'inverse' ('translate' pivot)
-- @
class Transform geo where
    transform :: Transformation -> geo -> geo

-- | Transform the result of a function.
--
-- @
-- moveRight :: 'Vec2' -> 'Vec2'
-- moveRight ('Vec2' x y) = 'Vec2' (x+1) y
--
-- moveRightThenRotate :: 'Vec2' -> 'Vec2'
-- moveRightThenRotate = 'transform' ('rotate' ('deg' 90)) moveRight
-- @
instance Transform b => Transform (a -> b) where
    transform t f = transform t . f

instance Transform Vec2 where
    transform (Transformation a b c
                              d e f)
              (Vec2 x y)
            = Vec2 (a*x + b*y + c) (d*x + e*y + f)

instance Transform Line where
    transform t (Line start end) = Line (transform t start) (transform t end)

instance Transform Polygon where
    transform t (Polygon ps) = Polygon (transform t ps)

-- | Identical to the 'Monoid' instance of 'Transformation'; see there for documentation.
instance Transform Transformation where
    transform = transformationProduct

instance Transform a => Transform [a] where
    transform t = map (transform t)

instance (Transform a, Transform b) => Transform (a,b) where
    transform t (a,b) = (transform t a, transform t b)

instance (Transform a, Transform b, Transform c) => Transform (a,b,c) where
    transform t (a,b,c) = (transform t a, transform t b, transform t c)

instance (Transform a, Transform b, Transform c, Transform d) => Transform (a,b,c,d) where
    transform t (a,b,c,d) = (transform t a, transform t b, transform t c, transform t d)

instance (Transform a, Transform b, Transform c, Transform d, Transform e) => Transform (a,b,c,d,e) where
    transform t (a,b,c,d,e) = (transform t a, transform t b, transform t c, transform t d, transform t e)

-- | Translate the argument by an offset given by the vector.
--
-- This effectively adds the 'Vec2' to all contained 'Vec2's in the target.
translate :: Vec2 -> Transformation
translate (Vec2 dx dy) = Transformation
    1 0 dx
    0 1 dy

-- | Rotate around zero in mathematically positive direction (counter-clockwise).
--
-- To rotate around a different point, use 'rotateAround'.
rotate :: Angle -> Transformation
rotate (Rad a) = Transformation
    (cos a) (-sin a) 0
    (sin a) ( cos a) 0

-- | Rotate around a point.
rotateAround :: Vec2 -> Angle -> Transformation
rotateAround pivot angle = translate pivot <> rotate angle <> inverse (translate pivot)

-- | Scale the geometry relative to zero, maintaining aspect ratio.
scale :: Double -> Transformation
scale x = scale' x x

-- | Scale the geometry with adjustable aspect ratio.
--
-- While being more general and mathematically more natural, this function is used
-- less in practice, hence it gets the prime in the name.
scale' :: Double -> Double -> Transformation
scale' x y = Transformation
    x 0 0
    0 y 0

-- | Scale the geometry relative to a point, maintaining aspect ratio.
scaleAround :: Vec2 -> Double -> Transformation
scaleAround pivot x = translate pivot <> scale x <> inverse (translate pivot)

-- | Scale the geometry relative to a point, with adjustable aspect ratio.
--
-- While being more general and mathematically more natural, this function is used
-- less in practice, hence it gets the prime in the name.
scaleAround' :: Vec2 -> Double -> Double -> Transformation
scaleAround' pivot x y = translate pivot <> scale' x y <> inverse (translate pivot)

-- | Mirror the geometry along a line.
--
-- This function is called 'mirrorAlong' and not @mirror@ since the latter makes a
-- very good name for arguments of this function.
mirrorAlong :: Line -> Transformation
mirrorAlong line@(Line p _) = translate p <> rotate angle <> mirrorYCoords <> inverse (rotate angle) <> inverse (translate p)
  where
    angle = angleOfLine line

-- | Invert all X coordinates.
--
-- NB: if it was called @mirrorX@ it wouldn’t be clear whether it mirrors the X
-- coordinates, or along the X axis, which would mirror the Y coordinates. The
-- longer name makes it clearer.
mirrorXCoords :: Transformation
mirrorXCoords = scale' (-1) 1

-- | Invert all Y coordinates.
mirrorYCoords :: Transformation
mirrorYCoords = scale' 1 (-1)




-- | The bounding box, with the minimum and maximum vectors.
--
-- In geometrical terms, the bounding box is a rectangle spanned by the top-left
-- (minimum) and bottom-right (maximum) points, so that everything is inside the
-- rectangle.
--
-- Make sure the first argument is smaller than the second when using the
-- constructor directly! Or better yet, don’t use the constructor and create
-- bounding boxes via the provided instances.
data BoundingBox = BoundingBox
    { _bbMin :: !Vec2 -- ^ Minimum coordinate (top left in Cairo coordinates)
    , _bbMax :: !Vec2 -- ^ Maximum coordinate (bottom right in Cairo coordinates)
    } deriving (Eq, Ord, Show)

instance NFData BoundingBox where rnf _ = ()

instance Semigroup BoundingBox where
    BoundingBox (Vec2 xMin1 yMin1) (Vec2 xMax1 yMax1) <> BoundingBox (Vec2 xMin2 yMin2) (Vec2 xMax2 yMax2)
      = BoundingBox (Vec2 (min xMin1 xMin2) (min yMin1 yMin2))
                    (Vec2 (max xMax1 xMax2) (max yMax1 yMax2))

instance Transform BoundingBox where
    transform t (BoundingBox lo hi) = BoundingBox (transform t lo) (transform t hi)

-- | A bounding box with the minimum at (plus!) infinity and maximum at (minus!)
-- infinity acts as a neutral element. This is mostly useful so we can make
-- potentiallly empty data structures such as @[a]@ and @'Maybe' a@ instances too.
instance Monoid BoundingBox where
    mempty = BoundingBox (Vec2 inf inf) (Vec2 (-inf) (-inf))
      where inf = 1/0

boundingBoxPolygon :: BoundingBox -> Polygon
boundingBoxPolygon bb = Polygon [Vec2 x1 y1, Vec2 x1 y2, Vec2 x2 y2, Vec2 x2 y1]
  where BoundingBox (Vec2 x1 y1) (Vec2 x2 y2) = bb

insideBoundingBox :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
insideBoundingBox thing bigObject =
    let thingBB = boundingBox thing
        bigObjectBB = boundingBox bigObject
    in bigObjectBB == bigObjectBB <> thingBB

boundingBoxCenter :: HasBoundingBox a => a -> Vec2
boundingBoxCenter x = let BoundingBox lo hi = boundingBox x in (lo+.hi)/.2

boundingBoxSize :: HasBoundingBox a => a -> (Double, Double)
boundingBoxSize x = (abs deltaX, abs deltaY)
  where
    BoundingBox lo hi = boundingBox x
    Vec2 deltaX deltaY = hi -. lo

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

-- | Do the bounding boxes of two objects overlap?
overlappingBoundingBoxes :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
overlappingBoundingBoxes a b = go (boundingBox a) (boundingBox b)
  where
    go (BoundingBox (Vec2 loAx loAy) (Vec2 hiAx hiAy)) (BoundingBox (Vec2 loBx loBy) (Vec2 hiBx hiBy))
        | loAx > hiBx = False -- A right of B
        | hiAx < loBx = False -- A left of B
        | loAy > hiBy = False -- A below B
        | hiAy < loBy = False -- A above B
        | otherwise = True

data ScalingBehavior
    = FitAllMaintainAspect
        -- ^ Maintain aspect ratio, possibly leaving some margin for one of the
        -- dimensions

    | FitAllIgnoreAspect
        -- ^ Fit the target, possibly stretching the source unequally in x/y
        -- directions

    | FitWidthMaintainAspect
        -- ^ Fit the entire width, with the height of the geometry potentially
        -- exceeding the target box.

    | FitHeightMaintainAspect
        -- ^ Fit the entire height, with the width of the geometry potentially
        -- exceeding the target box.

    deriving (Eq, Ord, Show)

-- | Generate a transformation that transforms the bounding box of one object to
-- match the other’s. Canonical use case: transform any part of your graphic to
-- fill the Cairo canvas.
transformBoundingBox
    :: (HasBoundingBox source, HasBoundingBox target)
    => source              -- ^ e.g. drawing coordinate system
    -> target              -- ^ e.g. Cairo canvas
    -> ScalingBehavior -- ^ Maintain or ignore aspect ratio
    -> Transformation
transformBoundingBox source target scalingBehavior
  = let bbSource = boundingBox source
        bbTarget = boundingBox target

        sourceCenter = boundingBoxCenter bbSource
        targetCenter = boundingBoxCenter bbTarget

        translateToMatchCenter = translate (targetCenter -. sourceCenter)

        (sourceWidth, sourceHeight) = boundingBoxSize bbSource
        (targetWidth, targetHeight) = boundingBoxSize bbTarget
        xScaleFactor = targetWidth / sourceWidth
        yScaleFactor = targetHeight / sourceHeight

        scaleToMatchSize = case scalingBehavior of
            FitAllMaintainAspect ->
                let scaleFactor = min xScaleFactor yScaleFactor
                in scaleAround targetCenter scaleFactor
            FitWidthMaintainAspect -> scaleAround targetCenter xScaleFactor
            FitHeightMaintainAspect -> scaleAround targetCenter yScaleFactor
            FitAllIgnoreAspect -> scaleAround' targetCenter xScaleFactor yScaleFactor

    in  scaleToMatchSize <> translateToMatchCenter

instance VectorSpace Vec2 where
    Vec2 x1 y1 +. Vec2 x2 y2 = Vec2 (x1+x2) (y1+y2)
    a *. Vec2 x y = Vec2 (a*x) (a*y)
    negateV (Vec2 x y) = Vec2 (-x) (-y)
    zero = Vec2 0 0

dotProduct :: Vec2 -> Vec2 -> Double
dotProduct (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

-- | Euclidean norm.
norm :: Vec2 -> Double
norm = sqrt . normSquare

-- | Squared Euclidean norm. Does not require a square root, and is thus
-- suitable for sorting points by distance without excluding certain kinds of
-- numbers such as rationals.
normSquare :: Vec2 -> Double
normSquare v = dotProduct v v

-- | Construct a 'Vec2' from polar coordinates
polar :: Angle -> Double -> Vec2
polar (Rad a) d = Vec2 (d * cos a) (d * sin a)

-- | Newtype safety wrapper.
--
-- Angles are not 'Ord', since the cyclic structure is very error-prone when
-- combined with comparisons in practice :-( Write your own comparators such as
-- @'comparing' 'getDeg'@ if you _really_ want to compare them directly. Often
-- times, using the 'dotProduct' (measure same-direction-ness) or cross product via
-- 'det' (measure leftness/rightness) is a much better choice to express what you
-- want.
newtype Angle = Rad Double
    deriving (Eq)

instance MWC.Uniform Angle where
    uniformM gen = fmap deg (MWC.uniformRM (0, 360) gen)

instance NFData Angle where rnf _ = ()

instance Show Angle where
    show (Rad r) = printf "deg %2.8f" (r / pi * 180)

instance VectorSpace Angle where
    Rad a +. Rad b = Rad (a + b)
    Rad a -. Rad b = Rad (a - b)
    a *. Rad b = Rad (a * b)
    negateV (Rad a) = Rad (-a)
    zero = Rad 0

-- | Degrees-based 'Angle' smart constructor.
deg :: Double -> Angle
deg degrees = Rad (degrees / 180 * pi)

-- | Radians-based 'Angle' smart constructor.
rad :: Double -> Angle
rad = Rad

-- | Get an angle’s value in degrees
getDeg :: Angle -> Double
getDeg (Rad r) = r / pi * 180

-- | Get an angle’s value in radians
getRad :: Angle -> Double
getRad (Rad r) = r

-- | Get the angle’s value, normalized to one revolution. This makes e.g. 720° mean
-- the same as 360°, which is otherwise not true for 'Angle's – turning twice might
-- be something else than turning once, after all.
normalizeAngle
    :: Angle -- ^ Interval start
    -> Angle -- ^ Angle to normalize
    -> Angle -- ^ Normalized angle [start ... start + one revolution]
normalizeAngle start (Rad r) = Rad (r `mod'` (2*pi)) -. start

-- | Directional vector of a line, i.e. the vector pointing from start to end.
-- The norm of the vector is the length of the line. Use 'normalizeLine' to make
-- it unit length.
vectorOf :: Line -> Vec2
vectorOf (Line start end) = end -. start

-- | Where do you end up when walking 'Distance' on a 'Line'?
--
-- @
-- moveAlong (Line start end) (Distance 0) == start
-- moveAlong (Line start end) (lineLength …) == end
-- @
moveAlongLine
    :: Line
    -> Double -- ^ Distance
    -> Vec2
moveAlongLine line@(Line start _end) d
  = let len = lineLength line
    in start +. (d/len) *. vectorOf line

-- | Angle of a single line, relative to the x axis.
angleOfLine :: Line -> Angle
angleOfLine (Line (Vec2 x1 y1) (Vec2 x2 y2)) = rad (atan2 (y2-y1) (x2-x1))

angleBetween :: Line -> Line -> Angle
angleBetween line1 line2
  = let Rad a1 = angleOfLine line1
        Rad a2 = angleOfLine line2
    in rad (a2 - a1)


angledLine
    :: Vec2   -- ^ Start
    -> Angle
    -> Double -- ^ Length
    -> Line
angledLine start angle len = Line start (start +. polar angle len)

lineLength :: Line -> Double
lineLength = norm . vectorOf

-- | Resize a line, keeping the starting point.
resizeLine :: (Double -> Double) -> Line -> Line
resizeLine f line@(Line start _end)
  = let v = vectorOf line
        len = norm v
        len' = f len
        v' = (len'/len) *. v
        end' = start +. v'
    in Line start end'

-- | Resize a line, keeping the middle point.
resizeLineSymmetric :: (Double -> Double) -> Line -> Line
resizeLineSymmetric f line@(Line start end) = (centerLine . resizeLine f . transform (translate delta)) line
  where
    middle = 0.5 *. (start +. end)
    delta = middle -. start

-- | Move the line so that its center is where the start used to be.
--
-- Useful for painting lines going through a point symmetrically.
centerLine :: Line -> Line
centerLine line@(Line start end) = transform (translate delta) line
  where
    middle = 0.5 *. (start +. end)
    delta = start -. middle

-- | Move the end point of the line so that it has length 1.
normalizeLine :: Line -> Line
normalizeLine = resizeLine (const 1)

-- | Distance of a point from a line.
distanceFromLine :: Vec2 -> Line -> Double
distanceFromLine (Vec2 ux uy) (Line p1@(Vec2 x1 y1) p2@(Vec2 x2 y2))
  = let l = norm (p2 -. p1)
    in abs ((x2-x1)*(y1-uy) - (x1-ux) * (y2-y1)) / l

-- | Direction vector of a line.
direction :: Line -> Vec2
direction = vectorOf . normalizeLine

-- | Switch defining points of a line.
lineReverse :: Line -> Line
lineReverse (Line start end) = Line end start

bugError :: String -> a
bugError msg = errorWithoutStackTrace (msg ++ "\nThis should never happen! Please report it as a bug.")

data LLIntersection
    = IntersectionReal Vec2
        -- ^ Two lines intersect fully.

    | IntersectionVirtualInsideL Vec2
        -- ^ The intersection is in the left argument (of 'intersectionLL')
        -- only, and only on the infinite continuation of the right argument.

    | IntersectionVirtualInsideR Vec2
        -- ^ dito, but the other way round.

    | IntersectionVirtual Vec2
        -- ^ The intersection lies in the infinite continuations of both lines.

    | Parallel
        -- ^ Lines are parallel

    | Collinear (Maybe Line)
        -- ^ Lines are on the same line, and maybe even overlap.

    deriving (Eq, Ord, Show)

intersectionPoint :: LLIntersection -> Maybe Vec2
intersectionPoint (IntersectionReal v)           = Just v
intersectionPoint (IntersectionVirtualInsideL v) = Just v
intersectionPoint (IntersectionVirtualInsideR v) = Just v
intersectionPoint (IntersectionVirtual v)        = Just v
intersectionPoint _                              = Nothing

-- | Calculate the intersection of two lines.
--
-- Returns the point of the intersection, and whether it is inside both, one, or
-- none of the provided finite line segments.
intersectionLL :: Line -> Line -> LLIntersection
intersectionLL lineL lineR
    = intersectionType
  where
    intersectionType
        | discriminant == 0 && det (v1 -. v2) (v1 -. v3) /= 0
          = Parallel
        | discriminant == 0
          = Collinear $ case fmap forwardness [v2, v3, v4] of
            ~[f2, f3, f4] | f3 >= f2 && f4 >= f2 -> Nothing
                          | f3 <= 0  && f4 <= 0  -> Nothing
                          | f3 <= 0  && f4 >= f2 -> Just lineL
                          | f4 <= 0  && f3 >= f2 -> Just lineL
                          | f3 <= 0              -> Just (Line v1 v4)
                          | f4 <= 0              -> Just (Line v1 v3)
                          | f3 >= f2             -> Just (Line v4 v2)
                          | f4 >= f2             -> Just (Line v3 v2)
                          | f4 >= f3             -> Just (Line v3 v4)
                          | otherwise            -> Just (Line v4 v3)
        | otherwise
          = case (intersectionInsideL, intersectionInsideR) of
            (True,  True)  -> IntersectionReal iPoint
            (True,  False) -> IntersectionVirtualInsideL iPoint
            (False, True)  -> IntersectionVirtualInsideR iPoint
            (False, False) -> IntersectionVirtual iPoint

    -- Calculation copied straight off of Wikipedia, then converted Latex to
    -- Haskell using bulk editing.
    --
    -- https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection

    Line v1 v2 = lineL
    Line v3 v4 = lineR

    discriminant = det (v1 -. v2) (v3 -. v4)

    iPoint = (det v1 v2 *. (v3 -. v4) -. det v3 v4 *. (v1 -. v2)) /. discriminant

    intersectionInsideL = sideOfLine lineR v1 /= sideOfLine lineR v2 || sideOfLine lineR v1 == EQ || sideOfLine lineR v2 == EQ
    intersectionInsideR = sideOfLine lineL v3 /= sideOfLine lineL v4 || sideOfLine lineL v3 == EQ || sideOfLine lineL v4 == EQ

    sideOfLine :: Line -> Vec2 -> Ordering
    sideOfLine (Line u v) p = compare (det (v -. u) (p -. u)) 0

    forwardness :: Vec2 -> Double
    forwardness v = dotProduct
        (direction lineL)
        (direction (Line v1 v))

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
    | signedPolygonArea polygon >= 0 = PolygonPositive
    | otherwise                      = PolygonNegative

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
            IntersectionReal _ -> True
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

    noIdenticalPoints (Polygon corners) = case nubOrd corners of
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
                         , IntersectionReal _ <- [intersectionLL edge1 edge2]
                         ]

-- | Average of polygon vertices
polygonAverage :: Polygon -> Vec2
polygonAverage (Polygon corners)
  = let (num, total) = foldl' (\(!n, !vec) corner -> (n+1, vec +. corner)) (0, Vec2 0 0) corners
    in (1/num) *. total

polygonCircumference :: Polygon -> Double
polygonCircumference poly = foldl'
    (\acc edge -> acc + lineLength edge)
    0
    (polygonEdges poly)

-- | Determinant of the matrix
--
-- > / x1 x2 \
-- > \ y1 y2 /
--
-- This is useful to calculate the (signed) area of the parallelogram spanned by
-- two vectors, or to check whether a vector is to the left or right of another
-- vector.
--
-- >>> det (Vec2 1 0) (Vec2 1 0) -- Colinear
-- 0
--
-- >>> det (Vec2 1 0) (Vec2 1 0.1) -- 2nd vec to the right of 1st (in Cairo coordinates)
-- 0.1
--
-- >>> det (Vec2 1 0) (Vec2 1 (-0.1)) -- 2nd vec to the left of 1st (in Cairo coordinates)
-- -0.1
det :: Vec2 -> Vec2 -> Double
det (Vec2 x1 y1) (Vec2 x2 y2) = x1*y2 - y1*x2

-- http://mathworld.wolfram.com/PolygonArea.html
polygonArea :: Polygon -> Double
polygonArea (Polygon ps)
  = let determinants = zipWith det ps (tail (cycle ps))
    in abs (sum determinants / 2)

signedPolygonArea :: Polygon -> Double
signedPolygonArea (Polygon ps)
  = let determinants = zipWith det ps (tail (cycle ps))
    in sum determinants / 2

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
    Line start0 end0 = transform (translate (negateV start)) line
    -- Rotate end point 90° CCW
    end0' = let Vec2 x y  = end0
            in Vec2 (-y) x
    -- Construct rotated line
    lineAt0' = Line start0 end0'
    -- Move line back so it goes through the point
    line' = transform (translate p) lineAt0'

-- | Optical reflection of a ray on a mirror. Note that the outgoing line has
-- reversed direction like light rays would. The second result element is the
-- point of intersection with the mirror, which is not necessarily on the line,
-- and thus returned separately.
reflection
    :: Line -- ^ Light ray
    -> Line -- ^ Mirror
    -> Maybe (Line, Vec2, LLIntersection)
            -- ^ Reflected ray; point of incidence; type of intersection of the
            -- ray with the mirror. The reflected ray is symmetric with respect
            -- to the incoming ray (in terms of length, distance from mirror,
            -- etc.), but has reversed direction (like real light).
reflection ray mirror
  = let iType = intersectionLL ray mirror
    in  case intersectionPoint iType of
        Nothing     -> Nothing
        Just iPoint -> Just (lineReverse ray', iPoint, iType)
          where
            mirrorAxis = perpendicularLineThrough iPoint mirror
            ray' = transform (mirrorAlong mirrorAxis) ray
