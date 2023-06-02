module Geometry.Core (
    -- * Primitives
    -- ** 2D Vectors
      Vec2(..)
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
    , intersectInfiniteLines
    , LLIntersection(..)
    , intersectionLL
    , intersectionPoint
    , subdivideLine
    , subdivideLineByLength
    , reflection

    -- ** Polylines
    , Polyline(..)
    , polylineLength
    , polylineEdges

    -- ** Polygons
    , Polygon(..)
    , normalizePolygon
    , PolygonError(..)
    , validatePolygon
    , pointInPolygon
    , polygonAverage
    , polygonCentroid
    , polygonCircumference
    , polygonArea
    , signedPolygonArea
    , polygonEdges
    , polygonAngles
    , isConvex
    , convexHull
    , PolygonOrientation(..)
    , polygonOrientation
    , growPolygon
    , shrinkPolygon

    -- ** Circles and ellipses
    , Circle(..)
    , UnsafeTransformCircle(..)
    , toEllipse
    , Ellipse(..)

    -- ** Angles
    , Angle
    , deg
    , getDeg
    , rad
    , getRad
    , normalizeAngle
    , pseudoAngle

    -- ** Vector arithmetic
    , VectorSpace(..)
    , vsum

    -- * Transformations
    , Transform(..)
    , Transformation(..)
    , NoTransform(..)
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
    , shear
    , decomposeTransformation

    -- * Bounding Box
    , HasBoundingBox(..)
    , BoundingBox(..)
    , NoBoundingBox(..)
    , overlappingBoundingBoxes
    , transformBoundingBox
    , FitDimension(..)
    , FitAspect(..)
    , FitAlign(..)
    , TransformBBSettings(..)
    , boundingBoxPolygon
    , insideBoundingBox
    , boundingBoxCenter
    , boundingBoxIntersection
    , boundingBoxSize
    , growBoundingBox
    , shrinkBoundingBox

    -- * Matrices
    , Mat2(..)
    , det
    , mulMV
    , mulVTM

    -- * Useful stuff
    , vectorOf
    , cross
    , direction
    , module Data.Sequential
    , Group(..)
) where



import           Algebra.Group
import           Algebra.VectorSpace
import           Control.DeepSeq
import           Control.Monad
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Default.Class
import           Data.Fixed
import           Data.Foldable
import           Data.List
import           Data.List.Extended
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Vector         (Vector)
import qualified System.Random.MWC   as MWC
import           Text.Printf

import Data.Sequential



-- $setup
-- >>> import           Control.Monad
-- >>> import           Draw
-- >>> import           Geometry.Algorithms.Sampling
-- >>> import qualified Graphics.Rendering.Cairo     as C
-- >>> import           Numerics.Interpolation
-- >>> import qualified System.Random.MWC.Extended   as MWC



data Vec2 = Vec2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Eq, Ord, Show)

instance NFData Vec2 where rnf _ = ()

instance MWC.UniformRange Vec2 where
    uniformRM (Vec2 xMin yMin, Vec2 xMax yMax) gen =
        Vec2 <$> MWC.uniformRM (xMin, xMax) gen <*> MWC.uniformRM (yMin, yMax) gen

-- | Explicit type for polylines. Useful in type signatures, beacuse [[[Vec2]]] is
-- really hard to read. Also makes some typeclass instances clearer, such as
-- 'sketch'.
newtype Polyline = Polyline [Vec2]

instance Eq Polyline where Polyline a == Polyline b = a == b

instance Ord Polyline where compare (Polyline a) (Polyline b) = compare a b

instance Show Polyline where show (Polyline xs) = "Polyline " ++ show xs

instance NFData Polyline where rnf (Polyline xs) = rnf xs

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
      = let Polygon p1_normalized = normalizePolygon p1
            Polygon p2_normalized = normalizePolygon p2
        in p1_normalized == p2_normalized

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
data Line = Line {-# UNPACK #-} !Vec2 {-# UNPACK #-} !Vec2 deriving (Eq, Ord, Show)

instance NFData Line where rnf _ = ()

-- | \(2\times2\) matrix.
data Mat2 = Mat2 !Double !Double !Double !Double
    -- ^ @'Mat2' a11 a12 a21 a22@ \(= \begin{pmatrix}a_{11} & a_{12}\\ a_{21} & a_{22}\end{pmatrix}\)
    deriving (Eq, Ord, Show)

-- | Multiply a matrix \(A\) with a (column) vector \(\mathbf b\).
--
-- \[
-- \sum_i\mathbf e_i c_i = \sum_i\mathbf e_i a_{ij} b_j
-- \quad\Leftrightarrow\quad
-- \begin{pmatrix}c_1\\c_2\end{pmatrix}
-- = \begin{pmatrix}a_{11}&a_{12}\\ a_{21}&a_{22}\end{pmatrix}
--       \begin{pmatrix}b_1\\ b_2\end{pmatrix}
-- = \begin{pmatrix}
--       a_{11}b_1+a_{12}b_2 \\
--       a_{21}b_1+a_{22}b_2
--   \end{pmatrix}
-- \]
mulMV :: Mat2 -> Vec2 -> Vec2
mulMV (Mat2 a11 a12 a21 a22) (Vec2 b1 b2) =
    Vec2 (a11*b1 + a12*b2)
         (a21*b1 + a22*b2)

-- | Multiply a (row) vector \(\mathbf a^\top\) with a matrix \(A\).
--
-- \[
-- \sum_i\mathbf e_i c_i = \sum_i b_i a_{ij} \mathbf e_j
-- \quad\Leftrightarrow\quad
-- \begin{pmatrix}c_1&c_2\end{pmatrix}
-- = \begin{pmatrix}b_1&b_2\end{pmatrix}
--       \begin{pmatrix}a_{11}&a_{12}\\ a_{21}&a_{22}\end{pmatrix}
-- = \begin{pmatrix}
--       b_1a_{11}+b_2a_{21} &
--       b_1a_{12}+b_2a_{22}
--   \end{pmatrix}
-- \]
mulVTM :: Vec2 -> Mat2 -> Vec2
mulVTM (Vec2 b1 b2) (Mat2 a11 a12 a21 a22) =
    Vec2 (b1*a11 + b2*a21)
         (b1*a12 + b2*a22)

instance Semigroup Mat2 where
    Mat2   a11 a12
           a21 a22
     <>
      Mat2 b11 b12
           b21 b22

     = Mat2 (a11*b11 + a12*b21) (a11*b12 + a12*b22)
            (a21*b11 + a22*b21) (a21*b12 + a22*b22)

instance Monoid Mat2 where
    mempty = Mat2 1 0
                  0 1

instance Group Mat2 where
    inverse (Mat2 a b
                    d e)
        = let x = 1 / (a*e - b*d)
        in  Mat2 (x*e)    (x*(-b))
                 (x*(-d)) (x*a)

instance VectorSpace Mat2 where
    Mat2 a11 a12 a21 a22 +. Mat2 b11 b12 b21 b22 = Mat2 (a11+b11) (a12+b12) (a21+b21) (a22+b22)
    Mat2 a11 a12 a21 a22 -. Mat2 b11 b12 b21 b22 = Mat2 (a11-b11) (a12-b12) (a21-b21) (a22-b22)
    s *. Mat2 a11 a12 a21 a22 = Mat2 (s *. a11) (s *. a12) (s *. a21) (s *. a22)
    zero = Mat2 0 0 0 0

instance NFData Mat2 where rnf _ = ()

-- | Affine transformation. Typically these are not written using the constructor
-- directly, but by combining functions such as 'translate' or 'rotateAround' using
-- '<>'.
--
-- \[
-- \begin{pmatrix}\mathbf{x'}\\1\end{pmatrix}
-- = \begin{pmatrix} A \mathbf x + \mathbf b\\1\end{pmatrix}
-- = \left(\begin{array}{c|c} A & \mathbf b \\ \hline 0 & 1\end{array}\right)
--   \begin{pmatrix}\mathbf x\\ 1\end{pmatrix}
-- \]
data Transformation =
    Transformation !Mat2 !Vec2
                    -- ^
                    -- > transformation (Mat2 a11 a12
                    -- >                      a21 a22)
                    -- >                (Vec2 b1 b2)
                    -- \(= \left(\begin{array}{cc|c} a_{11} & a_{12} & b_1 \\ a_{21} & a_{22} & b_2 \\ \hline 0 & 0 & 1\end{array}\right)\)
    deriving (Eq, Ord, Show)

instance NFData Transformation where rnf _ = ()

-- | The order transformations are applied in function order:
--
-- @
-- 'transform' ('scale' s <> 'translate' p)
-- ==
-- 'transform' ('scale' s) . 'transform' ('translate' p)
-- @
--
-- In other words, this first translates its argument, and then scales.
-- Note that Cairo does its Canvas transformations just the other way round, since
-- in Cairo you do not move the geometry, but the coordinate system. If you wrap a
-- transformation in 'inverse', you get the Cairo behavior.
instance Semigroup Transformation where
    Transformation m1 v1 <> Transformation m2 v2 = Transformation (m1 <> m2) (m1 `mulMV` v2 +. v1)

instance Monoid Transformation where
    mempty = Transformation mempty zero

instance Group Transformation where
    inverse (Transformation (Mat2 a b
                                  d e)
                            (Vec2 c f))
        = let x = 1 / (a*e - b*d)
        in Transformation (Mat2 (x*e)    (x*(-b))
                                (x*(-d)) (x*a))
                          (Vec2 (x*(-e*c + b*f))
                                (x*( d*c - a*f)))

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
    transform (Transformation (Mat2 a b
                                    d e)
                              (Vec2 c f))
              (Vec2 x y)
            = Vec2 (a*x + b*y + c) (d*x + e*y + f)

instance Transform Line where
    transform t (Line start end) = Line (transform t start) (transform t end)

instance Transform Polygon where
    transform t (Polygon ps) = Polygon (transform t ps)

instance Transform Polyline where transform t (Polyline xs) = Polyline (transform t xs)

instance Transform Transformation where
    transform = (<>)

-- | Points mapped to the same point will unify to a single entry
instance (Ord a, Transform a) => Transform (S.Set a) where
    transform t = S.map (transform t)

instance Transform a => Transform (M.Map k a) where
    transform t = fmap (transform t)

instance Transform a => Transform [a] where
    transform t = fmap (transform t)

instance Transform a => Transform (Vector a) where
    transform t = fmap (transform t)

instance (Transform a, Transform b) => Transform (Either a b) where
    transform t = bimap (transform t) (transform t)

instance (Transform a, Transform b) => Transform (a,b) where
    transform t (a,b) = (transform t a, transform t b)

instance (Transform a, Transform b, Transform c) => Transform (a,b,c) where
    transform t (a,b,c) = (transform t a, transform t b, transform t c)

instance (Transform a, Transform b, Transform c, Transform d) => Transform (a,b,c,d) where
    transform t (a,b,c,d) = (transform t a, transform t b, transform t c, transform t d)

instance (Transform a, Transform b, Transform c, Transform d, Transform e) => Transform (a,b,c,d,e) where
    transform t (a,b,c,d,e) = (transform t a, transform t b, transform t c, transform t d, transform t e)

-- | Translate the argument by an offset given by the vector. @'translate' ('Vec2' 0 0) = 'mempty'@.
--
-- \[
-- \text{translate}\begin{pmatrix}\Delta_x\\\Delta_y\end{pmatrix}
--     = \left(\begin{array}{cc|c} 1 & 0 & \Delta_x \\ 0 & 1 & \Delta_y \\ \hline 0 & 0 & 1\end{array}\right)
-- \]
--
-- This effectively adds the 'Vec2' to all contained 'Vec2's in the target.
--
-- <<docs/haddock/Geometry/Core/translate.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/translate.svg" 100 30 $ do
--     let point = Vec2 10 10
--         offset = Vec2 80 10
--         point' = transform (translate offset) point
--     sketch (Circle point 5)
--     sketch (Circle point' 5)
--     C.fill
--     setColor (mathematica97 1)
--     sketch (Arrow (Line point point') def) >> C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0x93f965ed
translate :: Vec2 -> Transformation
translate = Transformation mempty

-- | Rotate around 'zero' in mathematically positive direction (counter-clockwise). @'rotate' ('rad' 0) = 'mempty'@.
--
-- \[
-- \text{rotate}(\alpha) = \left(\begin{array}{cc|c} \cos(\alpha) & -\sin(\alpha) & 0 \\ \sin(\alpha) & \cos(\alpha) & 0 \\ \hline 0 & 0 & 1\end{array}\right)
-- \]
--
-- To rotate around a different point, use 'rotateAround'.
--
-- <<docs/haddock/Geometry/Core/rotate.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/rotate.svg" 100 70 $ do
--     let point = Vec2 90 10
--         angle = deg 30
--         point' = transform (rotate angle) point
--     cairoScope $ do
--         sketch (Circle point 5, Circle point' 5)
--         C.fill
--     setColor (mathematica97 1)
--     let line = Line zero point
--         line' = Line zero point'
--     cairoScope $ do
--         C.setDash [1,1] 0
--         sketch (line, line')
--         C.stroke
--     cairoScope $ do
--         let angle = angleOfLine line
--             angle' = angleOfLine line'
--         C.arc 0 0 (lineLength line) (getRad angle) (getRad angle')
--         sketch (Arrow (transform (rotateAround point' (deg 15)) (Line point point')) def{_arrowDrawBody=False})
--         C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0x980f5f
rotate :: Angle -> Transformation
rotate (Rad a) = Transformation m zero
  where
    m = Mat2 (cos a) (-sin a)
             (sin a) ( cos a)

-- | Rotate around a point.
rotateAround :: Vec2 -> Angle -> Transformation
rotateAround pivot angle = translate pivot <> rotate angle <> inverse (translate pivot)

-- | Scale the geometry relative to zero, maintaining aspect ratio.
--
-- <<docs/haddock/Geometry/Core/scale.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/scale.svg" 100 100 $ do
--     let square = Polygon [Vec2 10 10, Vec2 10 45, Vec2 45 45, Vec2 45 10]
--         square' = transform (scale 2) square
--     sketch square
--     C.stroke
--     setColor (mathematica97 1)
--     sketch square'
--     C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0x9126ef6
scale :: Double -> Transformation
scale x = scale' x x

-- | Scale the geometry with adjustable aspect ratio. @'scale'' 1 1 = 'mempty'@.
--
-- \[
-- \text{scale'}(s_x,s_y) = \left(\begin{array}{cc|c} s_x & 0 & 0 \\ 0 & s_y & 0 \\ \hline 0 & 0 & 1\end{array}\right)
-- \]
--
-- While being more general and mathematically more natural than 'scale', this
-- function is used less in practice, hence it gets the prime in the name.
scale' :: Double -> Double -> Transformation
scale' x y = Transformation m zero
  where
    m = Mat2 x 0
             0 y

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

-- | Shear with a factor along x/y axis relative to zero. @'shear' 0 0 = 'mempty'@.
--
-- \[
-- \text{shear}(p,q)
--     = \left(\begin{array}{cc|c} 1 & p & 0 \\ q & 1 & 0 \\ \hline 0 & 0 & 1\end{array}\right)
-- \]
--
-- <<docs/haddock/Geometry/Core/shear.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/shear.svg" 100 100 $ do
--     let square = Polygon [Vec2 10 10, Vec2 10 80, Vec2 50 80, Vec2 50 10]
--         square' = transform (shear 0.5 0.1) square
--     sketch square
--     C.stroke
--     setColor (mathematica97 1)
--     sketch square'
--     C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0xfd1224e4
shear
    :: Double
    -> Double
    -> Transformation
shear p q = Transformation m zero
  where
    m = Mat2 1 p
             q 1

-- | This type simply wraps its contents, and makes 'transform' do nothing.
-- It’s a very useful type when you want to e.g. resize the whole geometry given to
-- your rendering function, but it contains some non-geometrical render data, like
-- a timestamp for each shape.
newtype NoTransform a = NoTransform a
    deriving (Eq, Ord, Show, Read, Bounded)

instance NFData a => NFData (NoTransform a) where rnf (NoTransform x) = rnf x
instance Enum a => Enum (NoTransform a) where
    toEnum = NoTransform . toEnum
    fromEnum (NoTransform x) = fromEnum x
instance HasBoundingBox a => HasBoundingBox (NoTransform a) where boundingBox (NoTransform x) = boundingBox x
instance Semigroup a => Semigroup (NoTransform a) where NoTransform x <> NoTransform y = NoTransform (x <> y)
instance Monoid a => Monoid (NoTransform a) where mempty = NoTransform mempty
-- | Don’t transform the contents.
instance Transform (NoTransform a) where transform _ x = x

-- | Decompose an affine transformation into scale, shear, rotation and translation parts.
-- This composition is not unique, since scale, shear and rotating can be done in different orders.
--
-- Our choice of decomposition is:
--
-- \[
-- \left(\begin{array}{cc|c} a_{11} & a_{12} & b_1 \\ a_{21} & a_{22} & b_2 \\ \hline & & 1\end{array}\right)
-- =
-- \underbrace{\left(\begin{array}{cc|c} 1 & & \Delta_x \\ & 1 & \Delta_y \\ \hline & & 1\end{array}\right)}                                    _{\text{translate}(\Delta_x, \Delta_y)}
-- \underbrace{\left(\begin{array}{cc|c} s_x & & \\ & s_y & \\ \hline & & 1\end{array}\right)}                                                  _{\text{scale}'(s_x,s_y)}
-- \underbrace{\left(\begin{array}{cc|c} 1 & & \\ \sigma_y & 1 & \\ \hline & & 1\end{array}\right)}                                             _{\text{shear}(0, \sigma_y)}
-- \underbrace{\left(\begin{array}{cc|c} \cos(\varphi) & -\sin(\varphi) & \\ \sin(\varphi) & \cos(\varphi) & \\ \hline & & 1\end{array}\right)} _{\text{rotatate}(\varphi)}
-- \]
decomposeTransformation
    :: Transformation
    -> (Vec2, (Double, Double), Double, Angle) -- ^ \(\Delta\mathbf v, (\text{scale}_x,\text{scale}_y), \text{shear}_y, \varphi\)
decomposeTransformation (Transformation m@(Mat2 a b d e) cf) =
    -- Source: https://math.stackexchange.com/a/78165/21079
    let p = sqrt (a^2 + b^2)
        detM = det m
        r = detM / p
        q = (a*d + b*e) / detM
        phi = - atan2 b a -- minus because of left-handed Cairo coordinates
    in (cf, (p,r), q, rad phi)

-- | The bounding box, with the minimum and maximum vectors.
--
-- In geometrical terms, the bounding box is a rectangle spanned by the bottom-left
-- (minimum) and top-right (maximum) points, so that everything is inside the
-- rectangle.
--
-- __Invariant!__ Make sure the first argument is smaller than the second when
-- using the constructor directly! Or better yet, don’t use the constructor and
-- create bounding boxes via the provided instances; for a rectangle, simply use
-- @'boundingBox' (a,b)@ instead of @'BoundingBox' a b@.
data BoundingBox = BoundingBox !Vec2 !Vec2 deriving (Eq, Ord, Show)

instance NFData BoundingBox where rnf _ = ()

instance Semigroup BoundingBox where
    BoundingBox (Vec2 xMin1 yMin1) (Vec2 xMax1 yMax1) <> BoundingBox (Vec2 xMin2 yMin2) (Vec2 xMax2 yMax2)
      = BoundingBox (Vec2 (min xMin1 xMin2) (min yMin1 yMin2))
                    (Vec2 (max xMax1 xMax2) (max yMax1 yMax2))

instance Transform BoundingBox where
    transform t (BoundingBox lo hi) = BoundingBox (transform t lo) (transform t hi)

-- | A bounding box with the minimum at (plus!) infinity and maximum at (minus!)
-- infinity acts as a neutral element. This is mostly useful so we can make
-- potentiallly empty data structures such as @[a]@ and @'Maybe' a@ instances of
-- 'HasBoundingBox'.
instance Monoid BoundingBox where
    mempty = BoundingBox (Vec2 inf inf) (Vec2 (-inf) (-inf))
      where inf = 1/0

-- | This type simply wraps its contents, but reports its bounding box as 'mempty'.
-- It’s a very useful type when you want to e.g. resize the whole geometry given to
-- your rendering function, but it contains some non-geometrical render data, like
-- a timestamp for each shape.
newtype NoBoundingBox a = NoBoundingBox a
    deriving (Eq, Ord, Show, Read, Bounded)

instance NFData a => NFData (NoBoundingBox a) where rnf (NoBoundingBox x) = rnf x
instance Enum a => Enum (NoBoundingBox a) where
    toEnum = NoBoundingBox . toEnum
    fromEnum (NoBoundingBox x) = fromEnum x
-- | Contents are ignored, reporting an empty bounding box.
instance HasBoundingBox (NoBoundingBox a) where boundingBox = mempty
instance Semigroup a => Semigroup (NoBoundingBox a) where NoBoundingBox x <> NoBoundingBox y = NoBoundingBox (x <> y)
instance Monoid a => Monoid (NoBoundingBox a) where mempty = NoBoundingBox mempty
instance Transform a => Transform (NoBoundingBox a) where transform t (NoBoundingBox x) = NoBoundingBox (transform t x)

-- | The rectangle representing a 'BoundingBox', with positive orientation.
--
-- <<docs/haddock/Geometry/Core/boundingBoxPolygon.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/boundingBoxPolygon.svg" 200 200 $ do
--     let (w,h) = (200,200)
--     points <- C.liftIO $ MWC.withRng [] $ \gen -> do
--         let region = shrinkBoundingBox 20 [zero, Vec2 w h]
--         poissonDisc gen region 15 5
--     for_ points $ \p -> sketch (Circle p 3) >> C.fill
--     setColor (mathematica97 1)
--     sketch (boundingBoxPolygon points) >> C.setLineWidth 3 >> C.stroke
-- :}
-- Generated file: size 25KB, crc32: 0x40902165
boundingBoxPolygon :: HasBoundingBox object => object -> Polygon
boundingBoxPolygon object = Polygon [Vec2 x1 y1, Vec2 x2 y1, Vec2 x2 y2, Vec2 x1 y2]
  where BoundingBox (Vec2 x1 y1) (Vec2 x2 y2) = boundingBox object

-- $
-- >>> polygonOrientation (boundingBoxPolygon [zero, Vec2 10 10])
-- PolygonPositive

-- | Is the argument’s bounding box fully contained in another’s bounding box?
--
-- <<docs/haddock/Geometry/Core/insideBoundingBox.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/insideBoundingBox.svg" 200 200 $ do
--     let (w,h) = (200,200)
--         parentBB = shrinkBoundingBox 50 [zero, Vec2 w h]
--         paintCheck :: (HasBoundingBox object, Sketch object) => object -> C.Render ()
--         paintCheck object = do
--             when (not (object `insideBoundingBox` parentBB)) $ grouped (C.paintWithAlpha 0.2) $ do
--                 setColor (mathematica97 3)
--                 sketch (boundingBox object)
--                 C.stroke
--             sketch object
--             C.stroke
--     cairoScope $ C.setLineWidth 3 >> setColor (mathematica97 3) >> sketch (boundingBoxPolygon parentBB) >> C.stroke
--     setColor (mathematica97 0) >> paintCheck (Circle (Vec2 110 60) 20)
--     setColor (mathematica97 1) >> paintCheck (Circle (Vec2 80 110) 15)
--     setColor (mathematica97 2) >> paintCheck (Line (Vec2 20 40) (Vec2 60 90))
--     setColor (mathematica97 4) >> paintCheck (transform (translate (Vec2 130 130) <> scale 30) (Geometry.Shapes.regularPolygon 5))
-- :}
-- Generated file: size 5KB, crc32: 0x8c351375
insideBoundingBox :: (HasBoundingBox thing, HasBoundingBox bigObject) => thing -> bigObject -> Bool
insideBoundingBox thing bigObject =
    let thingBB = boundingBox thing
        bigObjectBB = boundingBox bigObject
    in bigObjectBB == bigObjectBB <> thingBB

-- | Center/mean/centroid of a bounding box.
--
-- <<docs/haddock/Geometry/Core/boundingBoxCenter.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/boundingBoxCenter.svg" 200 200 $ do
--     let (w,h) = (200,200)
--     points <- C.liftIO $ MWC.withRng [] $ \gen -> do
--         let region = shrinkBoundingBox 20 [zero, Vec2 w h]
--         poissonDisc gen region 15 5
--     let pointsBB = boundingBox points
--     for_ points $ \p -> sketch (Circle p 3) >> C.fill
--     setColor (mathematica97 1)
--     sketch (boundingBoxPolygon pointsBB)
--     sketch (Cross (boundingBoxCenter pointsBB) 10)
--     sketch (Circle (boundingBoxCenter pointsBB) 10)
--     C.stroke
-- :}
-- Generated file: size 25KB, crc32: 0x31972ee1
boundingBoxCenter :: HasBoundingBox a => a -> Vec2
boundingBoxCenter x = let BoundingBox lo hi = boundingBox x in (lo+.hi)/.2

-- | Bounding box of the intersection of two bounding boxes. This is the
-- intersection analogon to '<>' representing union.
--
-- <<docs/haddock/Geometry/Core/boundingBoxIntersection.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/boundingBoxIntersection.svg" 200 200 $ do
--     let (w,h) = (200,200)
--     (p1s, p2s) <- C.liftIO $ MWC.withRng [] $ \gen -> do
--         let region1 = shrinkBoundingBox 20 [zero, Vec2 150 150]
--         p1s <- poissonDisc gen region1 15 5
--         let region2 = shrinkBoundingBox 20 [Vec2 50 50, Vec2 200 200]
--         p2s <- poissonDisc gen region2 15 5
--         pure (p1s, p2s)
--     for_ p1s $ \p -> sketch (Circle p 3) >> setColor (mathematica97 0) >> C.fill
--     for_ p2s $ \p -> sketch (Circle p 3) >> setColor (mathematica97 1) >> C.fill
--     sketch (fmap boundingBoxPolygon (boundingBoxIntersection p1s p2s)) >> setColor (mathematica97 3) >> C.stroke
-- :}
-- Generated file: size 25KB, crc32: 0x72cfba9f
boundingBoxIntersection
    :: (HasBoundingBox a, HasBoundingBox b)
    => a
    -> b
    -> Maybe BoundingBox -- ^ 'Nothing' if the input boxes don’t have finite overlap.
boundingBoxIntersection a b =
    let BoundingBox (Vec2 aMinX aMinY) (Vec2 aMaxX aMaxY) = boundingBox a
        BoundingBox (Vec2 bMinX bMinY) (Vec2 bMaxX bMaxY) = boundingBox b

        minX = max aMinX bMinX
        minY = max aMinY bMinY

        maxX = min aMaxX bMaxX
        maxY = min aMaxY bMaxY

    in if
        | minX >= maxX -> Nothing
        | minY >= maxY -> Nothing
        | otherwise -> Just (BoundingBox (Vec2 minX minY) (Vec2 maxX maxY))

-- | Width and height of a 'BoundingBox'.
boundingBoxSize :: HasBoundingBox a => a -> (Double, Double)
boundingBoxSize x = (abs deltaX, abs deltaY)
  where
    BoundingBox lo hi = boundingBox x
    Vec2 deltaX deltaY = hi -. lo

-- | Grow the bounding box by moving all its bounds outwards by a specified amount.
-- Useful to introduce margins. Negative values shrink instead; 'shrinkBoundingBox'
-- is a convenience wrapper for this case.
--
-- <<docs/haddock/Geometry/Core/growBoundingBox.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/growBoundingBox.svg" 200 200 $ do
--     let (w,h) = (200,200)
--     let bb = shrinkBoundingBox 40 [zero, Vec2 w h]
--     for_ [0,5..25] $ \amount -> cairoScope $ do
--         when (amount == 0) (C.setLineWidth 3)
--         sketch (boundingBoxPolygon (growBoundingBox (fromIntegral amount) bb))
--         setColor (icefire (Numerics.Interpolation.lerp (0,25) (0.5, 1) (fromIntegral amount)))
--         C.stroke
-- :}
-- Generated file: size 4KB, crc32: 0xedcda2c4
growBoundingBox
    :: HasBoundingBox boundingBox
    => Double -- ^ Amount \(x\) to move each side. Note that e.g. the total width will increase by \(2\times x\).
    -> boundingBox
    -> BoundingBox
growBoundingBox delta stuff  =
    let margin = Vec2 delta delta
        BoundingBox lo hi = boundingBox stuff
    in boundingBox [lo -. margin, hi +. margin]

-- | Convenience function for 'growBoundingBox' with a negative amount.
--
-- <<docs/haddock/Geometry/Core/shrinkBoundingBox.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/shrinkBoundingBox.svg" 200 200 $ do
--     let (w,h) = (200,200)
--     let bb = shrinkBoundingBox 40 [zero, Vec2 w h]
--     for_ [0,5..25] $ \amount -> cairoScope $ do
--         when (amount == 0) (C.setLineWidth 3)
--         sketch (boundingBoxPolygon (shrinkBoundingBox (fromIntegral amount) bb))
--         setColor (icefire (Numerics.Interpolation.lerp (0,25) (0.5, 0) (fromIntegral amount)))
--         C.stroke
-- :}
-- Generated file: size 4KB, crc32: 0xe0236688
shrinkBoundingBox :: HasBoundingBox boundingBox => Double -> boundingBox -> BoundingBox
shrinkBoundingBox delta = growBoundingBox (-delta)

-- | Anything we can paint has a bounding box. Knowing it is useful to e.g. rescale
-- the geometry to fit into the canvas or for collision detection.
class HasBoundingBox a where
    boundingBox :: a -> BoundingBox

-- | This is simply 'id', so a bounding box that doesn’t satisfy the min/max invariant will remain incorrect.
instance HasBoundingBox BoundingBox where
    boundingBox = id

instance HasBoundingBox Vec2 where
    boundingBox v = BoundingBox v v

instance HasBoundingBox a => HasBoundingBox (Maybe a) where
    boundingBox = foldMap boundingBox

instance (HasBoundingBox a, HasBoundingBox b) => HasBoundingBox (Either a b) where
    boundingBox = bifoldMap boundingBox boundingBox

instance (HasBoundingBox a, HasBoundingBox b) => HasBoundingBox (a,b) where
    boundingBox (a,b) = boundingBox a <> boundingBox b

instance (HasBoundingBox a, HasBoundingBox b, HasBoundingBox c) => HasBoundingBox (a,b,c) where
    boundingBox (a,b,c) = boundingBox a <> boundingBox b <> boundingBox c

instance (HasBoundingBox a, HasBoundingBox b, HasBoundingBox c, HasBoundingBox d) => HasBoundingBox (a,b,c,d) where
    boundingBox (a,b,c,d) = boundingBox a <> boundingBox b <> boundingBox c <> boundingBox d

instance (HasBoundingBox a, HasBoundingBox b, HasBoundingBox c, HasBoundingBox d, HasBoundingBox e) => HasBoundingBox (a,b,c,d,e) where
    boundingBox (a,b,c,d,e) = boundingBox a <> boundingBox b <> boundingBox c <> boundingBox d <> boundingBox e

instance (HasBoundingBox a) => HasBoundingBox [a] where
    boundingBox = foldMap boundingBox

instance (HasBoundingBox a) => HasBoundingBox (Vector a) where
    boundingBox = foldMap boundingBox

instance (HasBoundingBox a) => HasBoundingBox (S.Set a) where
    boundingBox = foldMap boundingBox

instance (HasBoundingBox a) => HasBoundingBox (M.Map k a) where
    boundingBox = foldMap boundingBox

instance HasBoundingBox Line where
    boundingBox (Line start end) = boundingBox (start, end)

instance HasBoundingBox Polygon where
    boundingBox (Polygon ps) = boundingBox ps

instance HasBoundingBox Polyline where boundingBox (Polyline xs) = boundingBox xs

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

data FitDimension
    = FitWidthHeight -- ^ Fit both width and height
    | FitWidth       -- ^ Fit width, ignoring what happens to the height (allow y stretching/compression)
    | FitHeight      -- ^ Fit height, ignoring what happens to the width  (allow x stretching/compression)
    | FitNone        -- ^ Don't fit dimensions, only align
    deriving (Eq, Ord, Show)

data FitAspect
    = MaintainAspect -- ^ Maintain width:height aspect ratio
    | IgnoreAspect   -- ^ Ignore aspect ratio
    deriving (Eq, Ord, Show)

data FitAlign
    = FitAlignCenter      -- ^ Align the centers of the results
    | FitAlignTopLeft     -- ^ Align the top left of the results
    | FitAlignTopRight    -- ^ Align the top right of the results
    | FitAlignBottomLeft  -- ^ Align the bottom left of the results
    | FitAlignBottomRight -- ^ Align the bottom right of the results
    deriving (Eq, Ord, Show)

-- | 'transformBoundingBox' settings paramter. If you don’t care for the details, use 'def'.
data TransformBBSettings = TransformBBSettings
    { _bbFitDimension :: FitDimension
    , _bbFitAspect    :: FitAspect
    , _bbFitAlign     :: FitAlign
    } deriving (Eq, Ord, Show)

-- | Fit width+height, maintaining aspect ratio, and matching centers.
instance Default TransformBBSettings where
    def = TransformBBSettings FitWidthHeight MaintainAspect FitAlignCenter

-- | Generate a transformation that transforms the bounding box of one object to
-- match the other’s. Canonical use case: transform any part of your graphic to
-- fill the Cairo canvas.
transformBoundingBox
    :: (HasBoundingBox source, HasBoundingBox target)
    => source -- ^ e.g. drawing coordinate system
    -> target -- ^ e.g. Cairo canvas
    -> TransformBBSettings
    -> Transformation
transformBoundingBox source target (TransformBBSettings fitDimension fitAspect fitAlign)
  = let bbSource@(BoundingBox sourceTopLeft sourceBottomRight) = boundingBox source
        bbTarget@(BoundingBox targetTopLeft targetBottomRight) = boundingBox target

        sourceCenter = boundingBoxCenter bbSource
        targetCenter = boundingBoxCenter bbTarget

        sourceBottomLeft =
            let Vec2 x _ = sourceTopLeft
                Vec2 _ y = sourceBottomRight
            in Vec2 x y
        targetBottomLeft =
            let Vec2 x _ = targetTopLeft
                Vec2 _ y = targetBottomRight
            in Vec2 x y
        sourceTopRight =
            let Vec2 x _ = sourceBottomRight
                Vec2 _ y = sourceTopLeft
            in Vec2 x y
        targetTopRight =
            let Vec2 x _ = targetBottomRight
                Vec2 _ y = targetTopLeft
            in Vec2 x y

        (scalePivot, translationOffset) = case fitAlign of
            FitAlignCenter      -> (targetCenter,      targetCenter      -. sourceCenter)
            FitAlignTopLeft     -> (targetTopLeft,     targetTopLeft     -. sourceTopLeft)
            FitAlignTopRight    -> (targetTopRight,    targetTopRight    -. sourceTopRight)
            FitAlignBottomLeft  -> (targetBottomLeft,  targetBottomLeft  -. sourceBottomLeft)
            FitAlignBottomRight -> (targetBottomRight, targetBottomRight -. sourceBottomRight)

        (sourceWidth, sourceHeight) = boundingBoxSize bbSource
        (targetWidth, targetHeight) = boundingBoxSize bbTarget
        xScaleFactor = targetWidth / sourceWidth
        yScaleFactor = targetHeight / sourceHeight

        scaleToMatchSize = case (fitDimension, fitAspect) of
            (FitWidthHeight, MaintainAspect) -> let scaleFactor = min xScaleFactor yScaleFactor in scaleAround scalePivot scaleFactor
            (FitWidth,       MaintainAspect) -> scaleAround scalePivot xScaleFactor
            (FitHeight,      MaintainAspect) -> scaleAround scalePivot yScaleFactor
            (FitWidthHeight, IgnoreAspect) -> scaleAround' scalePivot xScaleFactor yScaleFactor
            (FitWidth,       IgnoreAspect) -> scaleAround' scalePivot xScaleFactor 1
            (FitHeight,      IgnoreAspect) -> scaleAround' scalePivot 1 yScaleFactor
            (FitNone,        _)            -> mempty

    in scaleToMatchSize <> translate translationOffset

instance VectorSpace Vec2 where
    Vec2 x1 y1 +. Vec2 x2 y2 = Vec2 (x1+x2) (y1+y2)
    Vec2 x1 y1 -. Vec2 x2 y2 = Vec2 (x1-x2) (y1-y2)
    a *. Vec2 x y = Vec2 (a*x) (a*y)
    Vec2 x y /. a = Vec2 (x/a) (y/a)
    negateV (Vec2 x y) = Vec2 (-x) (-y)
    zero = Vec2 0 0

-- | Dot product.
--
-- \[ \begin{pmatrix}a \\ b\end{pmatrix} \cdot \begin{pmatrix}x \\ y\end{pmatrix}\ = ax + by\]
--
-- Can be used to check whether two vectors point into the same direction (product > 0).
dotProduct :: Vec2 -> Vec2 -> Double
dotProduct (Vec2 x1 y1) (Vec2 x2 y2) = x1*x2 + y1*y2

-- | Euclidean norm.
--
-- \[ \|\mathbf v\| = \sqrt{v_x^2 + v_y^2} \]
norm :: Vec2 -> Double
norm = sqrt . normSquare

-- | Squared Euclidean norm. Does not require a square root, and is thus
-- suitable for sorting points by distance without excluding certain kinds of
-- numbers such as rationals.
--
-- \[ \|\mathbf v\|^2 = v_x^2 + v_y^2 \]
normSquare :: Vec2 -> Double
normSquare v = dotProduct v v

-- | Construct a 'Vec2' from polar coordinates.
--
-- <<docs/haddock/Geometry/Core/polar.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/polar.svg" 100 60 $ do
--     let angle = deg 30
--         p = polar angle 100
--     cairoScope $ do
--         setColor (mathematica97 1)
--         C.arc 0 0 40 0 (getRad angle)
--         sketch (Line zero p)
--         C.stroke
--     cairoScope $ do
--         setColor (mathematica97 0)
--         sketch (Circle p 3)
--         C.fill
-- :}
-- Generated file: size 2KB, crc32: 0x7a43a888
polar :: Angle -> Double -> Vec2
polar (Rad a) d = Vec2 (d * cos a) (d * sin a)

-- | Newtype safety wrapper.
--
-- Angles are not 'Ord', since the cyclic structure is very error-prone when
-- combined with comparisons and 'VectorSpace' arithmetic in practice :-( Often
-- times, using the 'dotProduct' (measure same-direction-ness) or cross product via
-- 'det' (measure leftness/rightness) is a much better choice to express what you
-- want.
--
-- For sorting a number of points by angle, use the fast 'pseudoAngle' function.
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
    -> Angle -- ^ Angle normalized to the interval [start, start + deg 360)
normalizeAngle start a = rad (getRad (a -. start) `rem'` (2*pi)) +. start
  where x `rem'` m = (x `mod'` m + m) `mod'` m

-- | Increases monotonically with angle. Useful pretty much only to sort points by
-- angle, but this it does particularly fast (in particular, it’s much faster than
-- 'atan2').
--
-- Here is a comparison between 'atan2'-based (blue) and
-- pseudoAtan2/'pseudoAngle'-based angle of \([0\ldots 360\deg)\). Both are
-- increasing monotonically over the entire interval, hence yield the same
-- properties when it comes to 'Ord' and sorting in particular.
--
-- <<docs/haddock/Geometry/Core/pseudo_angle.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/pseudo_angle.svg" 200 100 $ do
--     let anglesDeg = takeWhile (< 180) [-180, -175 ..]
--         atan2Plot = Polyline $ do
--             angleDeg <- anglesDeg
--             let Vec2 x y = polar (deg angleDeg) 1
--             pure (Vec2 angleDeg (atan2 y x))
--         pseudoAtan2Plot = Polyline $ do
--             angleDeg <- anglesDeg
--             let vec = polar (deg angleDeg) 1
--             pure (Vec2 angleDeg (pseudoAngle vec))
--         trafo = transformBoundingBox
--             [atan2Plot, pseudoAtan2Plot]
--             (shrinkBoundingBox 10 [zero, Vec2 200 100])
--             def {_bbFitAspect = IgnoreAspect}
--     cairoScope $ do
--         sketch (transform trafo atan2Plot)
--         setColor (mathematica97 0)
--         C.stroke
--     cairoScope $ do
--         sketch (transform trafo pseudoAtan2Plot)
--         setColor (mathematica97 1)
--         C.stroke
-- :}
-- Generated file: size 5KB, crc32: 0xda270ddb
pseudoAngle :: Vec2 -> Double
pseudoAngle (Vec2 x y) = pseudoAtan2 y x

-- | Source of this nice alg:
-- https://vegard.wiki/w/Pseudoangles
pseudoAtan2 :: Double -> Double -> Double
pseudoAtan2 y x =  signum y * (1 - x / (abs x + abs y))

-- | Directional vector of a line, i.e. the vector pointing from start to end. The
-- norm of the vector is the length of the line. Use 'direction' if you need a
-- result of length 1.
vectorOf :: Line -> Vec2
vectorOf (Line start end) = end -. start
{-# INLINE vectorOf #-}

-- | Where do you end up when walking 'Distance' on a 'Line'?
--
-- @
-- moveAlong (Line start end) 0 == start
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
--
-- For sorting by angle, use the faster 'pseudoAngle'!
angleOfLine :: Line -> Angle
angleOfLine (Line (Vec2 x1 y1) (Vec2 x2 y2)) = rad (atan2 (y2-y1) (x2-x1))

-- | Angle between two lines.
--
-- The result depends on the direction of the lines; use 'lineReverse' if
-- necessary.
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
centerLine (Line start end) =
    let middle = 0.5 *. (start +. end)
        end' = middle
        start' = start -. end +. middle
    in Line start' end'

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
--
-- <<docs/haddock/Geometry/Core/line_reverse.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/line_reverse.svg" 100 60 $ do
--     let line = Line (Vec2 10 10) (Vec2 70 50)
--         line' = lineReverse line
--     cairoScope $ do
--         sketch (Arrow line def)
--         C.stroke
--     cairoScope $ do
--         setColor (mathematica97 1)
--         C.translate 20 0
--         sketch (Arrow line' def)
--         C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0xc8ad41ff
lineReverse :: Line -> Line
lineReverse (Line start end) = Line end start

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
        -- ^ Lines are parallel.

    | Collinear (Maybe Line)
        -- ^ Lines are collinear, and maybe overlap along a 'Line' segment.

    deriving (Eq, Ord, Show)

-- | Intersection of two infinite lines. Fast, but does not provide any error
-- handling (such as the parallel inputs case) and does not provide any detail
-- about the nature of the intersection point.
--
-- Use 'intersectionLL' for more detailed information, at the cost of computational
-- complexity.
intersectInfiniteLines :: Line -> Line -> Vec2
intersectInfiniteLines (Line (Vec2 x1 y1) (Vec2 x2 y2)) (Line (Vec2 x3 y3) (Vec2 x4 y4)) = Vec2 x y
  where
    x = ((x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4)) / ((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))
    y = ((x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4)) / ((x1-x2)*(y3-y4)-(y1-y2)*(x3-x4))

-- | The single point of intersection of two lines, or 'Nothing' for none (collinear).
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
--
-- See 'intersectInfiniteLines' for a more performant, but less nuanced result.
intersectionLL :: Line -> Line -> LLIntersection
intersectionLL lineL lineR
    = intersectionType
  where
    intersectionType
        | discriminant == 0 && cross (v1 -. v2) (v1 -. v3) /= 0
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

    discriminant = cross (v1 -. v2) (v3 -. v4)

    iPoint = (cross v1 v2 *. (v3 -. v4) -. cross v3 v4 *. (v1 -. v2)) /. discriminant

    intersectionInsideL =
        let sol1 = sideOfLine lineR v1
            sol2 = sideOfLine lineR v2
        in sol1 /= sol2 || sol1 == EQ || sol2 == EQ
    intersectionInsideR =
        let sol3 = sideOfLine lineL v3
            sol4 = sideOfLine lineL v4
        in sol3 /= sol4 || sol3 == EQ || sol4 == EQ

    sideOfLine :: Line -> Vec2 -> Ordering
    sideOfLine (Line u v) p = compare (cross (v -. u) (p -. u)) 0

    forwardness :: Vec2 -> Double
    forwardness v = dotProduct
        (direction lineL)
        (direction (Line v1 v))

-- | Subdivide a line into a number of equally long parts. Useful for
-- distorting straight lines. The first and last points are (exactly) equal to the
-- start and end of the input line.
--
-- See also 'subdivideLineByLength.'.
--
-- <<docs/haddock/Geometry/Core/subdivide_line.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/subdivide_line.svg" 200 150 $ do
--     let line = Line (Vec2 20 20) (Vec2 190 140)
--         subdivisions = subdivideLine 8 line
--     cairoScope $ do
--         C.setLineWidth 2
--         setColor (mathematica97 0)
--         sketch line
--         C.stroke
--     cairoScope $ for_ subdivisions $ \point -> do
--         sketch (Circle point 4)
--         setColor (mathematica97 1)
--         C.fill
-- :}
-- Generated file: size 4KB, crc32: 0x527103e6
subdivideLine :: Int -> Line -> [Vec2]
subdivideLine _ (Line start end) | start == end = [start, end]
subdivideLine numSegments line@(Line start end) = do
    let v = vectorOf line
    segment <- [0..numSegments]
    pure $ if | segment == 0 -> start
              | segment == numSegments -> end
              | otherwise -> let fraction = fromIntegral segment / fromIntegral numSegments
                             in start +. fraction*.v

-- | Subdivide a line into evenly-sized intervals of a maximum length. Useful for
-- distorting straight lines. The first and last points are (exactly) equal to the
-- start and end of the input line.
--
-- See also 'subdivideLine.'.
--
-- <<docs/haddock/Geometry/Core/subdivide_line_by_length.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/subdivide_line_by_length.svg" 200 150 $ do
--     let line = Line (Vec2 20 20) (Vec2 190 140)
--         subdivisions = subdivideLineByLength 30 line
--     cairoScope $ do
--         C.setLineWidth 2
--         setColor (mathematica97 0)
--         sketch line
--         C.stroke
--     cairoScope $ for_ subdivisions $ \point -> do
--         sketch (Circle point 4)
--         setColor (mathematica97 1)
--         C.fill
-- :}
-- Generated file: size 4KB, crc32: 0x6180122d
subdivideLineByLength
    :: Double -- ^ Maximum segment length
    -> Line
    -> [Vec2]
subdivideLineByLength segmentLength line =
    let numSegments = ceiling (lineLength line / segmentLength)
    in subdivideLine numSegments line

-- | All the polygon’s edges, in order, starting at an arbitrary corner.
--
-- <<docs/haddock/Geometry/Core/polygon_edges.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/polygon_edges.svg" 100 100 $ do
--     let polygon = Polygon [ transform (rotateAround (Vec2 50 50) (deg d)) (Vec2 50 10) | d <- take 5 [0, 360/5 ..] ]
--     for_ (zip [0..] (polygonEdges polygon)) $ \(i, edge) -> do
--         C.setLineCap C.LineCapRound
--         setColor (mathematica97 i)
--         sketch edge
--         C.stroke
-- :}
-- Generated file: size 3KB, crc32: 0x9c27134b
polygonEdges :: Polygon -> [Line]
polygonEdges (Polygon ps) = zipWith Line ps (tail (cycle ps))

-- | All interior angles, in order, starting at an arbitrary corner.
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
-- <<docs/haddock/Geometry/Core/convex_hull.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/convex_hull.svg" 200 200 $ do
--     let (w,h) = (200,200)
--     points <- C.liftIO $ MWC.withRng [] $ \gen ->
--         gaussianDistributedPoints
--             gen
--             (shrinkBoundingBox 10 [zero, Vec2 w h])
--             (30 *. mempty)
--             100
--     for_ points $ \point -> do
--         sketch (Circle point 3)
--         C.fill
--     setColor (mathematica97 1)
--     for_ (polygonEdges (convexHull points)) $ \edge ->
--         sketch (Arrow edge def{_arrowheadRelPos=0.5, _arrowheadSize=5})
--     C.stroke
-- :}
-- Generated file: size 29KB, crc32: 0xe4bf922
convexHull :: Foldable list => list Vec2 -> Polygon
-- Andrew’s algorithm
convexHull points
  = let pointsSorted = sort (toList points)
        angleSign a b c = signum (cross (b -. a) (c -. b))
        go :: (Double -> Double -> Bool) -> [Vec2] -> [Vec2] -> [Vec2]
        go cmp [] (p:ps) = go cmp [p] ps
        go cmp [s] (p:ps) = go cmp [p,s] ps
        go cmp (s:t:ack) (p:ps)
            | angleSign t s p `cmp` 0 = go cmp (p:s:t:ack) ps
            | otherwise = go cmp (t:ack) (p:ps)
        go _ stack [] = stack

    in Polygon (drop 1 (go (<=) [] pointsSorted) ++ drop 1 (reverse (go (>=) [] pointsSorted)))

-- | Orientation of a polygon
data PolygonOrientation
    = PolygonPositive -- ^ Counter-clockwise when plotted on a standard math coordinate system
    | PolygonNegative -- ^ Clockwise
    deriving (Eq, Ord, Show)

-- |
--
-- >>> polygonOrientation (Polygon [Vec2 0 0, Vec2 100 0, Vec2 100 100, Vec2 0 100])
-- PolygonPositive
--
-- >>> polygonOrientation (Polygon [Vec2 0 0, Vec2 0 100, Vec2 100 100, Vec2 100 0])
-- PolygonNegative
polygonOrientation :: Polygon -> PolygonOrientation
polygonOrientation polygon
    | signedPolygonArea polygon >= 0 = PolygonPositive
    | otherwise                      = PolygonNegative

-- | Circles are not an instance of 'Transform', because e.g. 'shear'ing a circle
-- yields an 'Ellipse'. To transform circles, convert them to an ellipse first with
-- 'toEllipse'.
data Circle = Circle
    { _circleCenter :: !Vec2
    , _circleRadius :: !Double
    } deriving (Eq, Ord, Show)

-- | Unit circle
instance Default Circle where
    def = Circle zero 1

instance NFData Circle where rnf _ = ()

instance HasBoundingBox Circle where
    boundingBox (Circle center r) = boundingBox (center -. Vec2 r r, center +. Vec2 r r)

-- | This allows applying a 'Transformation' to a 'Circle', which can e.g. be
-- useful to put circles in a 'transformBoundingBox' operation. See the 'Transform'
-- instance for details.
newtype UnsafeTransformCircle = UnsafeTransformCircle Circle

-- | Transform the circle as much as circles allow us. In order for the new circle
-- to have correct radius, the transformation must not contain:
--
--   * Shears (would yield an ellipse)
--   * Scaling by different amounts in x/y directions (dito) except mirroring
--
-- This instance is unsafe in the sense that it will yield a wrong result if these
-- requirements are not met, but it can be useful to do aspect ratio preserving
-- scales or translations of circles.
instance Transform UnsafeTransformCircle where
    transform t (UnsafeTransformCircle (Circle center radius)) =
        let center' = transform t center
            radius' = abs scaleX * radius
            (_, (scaleX, _), _shear, _angle) = decomposeTransformation t
        in UnsafeTransformCircle (Circle center' radius')

-- | Embedding of 'Circle' as a special case of an 'Ellipse'.
toEllipse :: Circle -> Ellipse
toEllipse (Circle center radius) = Ellipse (translate center <> scale radius)

-- | An 'Ellipse' is a 'Transformation' applied to the unit 'Circle'. Create them
-- using 'toEllipse' and by then applying 'Transformation's to it.
--
-- <<docs/haddock/Geometry/Core/ellipses.svg>>
--
-- === __(image code)__
-- >>> :{
-- >>> haddockRender "Geometry/Core/ellipses.svg" 300 300 $ do
-- >>>    let (w,h) = (300,300)
-- >>>        center = zero
-- >>>        radius = w/6*0.9
-- >>>        ellipse = toEllipse (Circle center radius)
-- >>>        grid i j = C.translate (fromIntegral i*w/3 + w/6) (fromIntegral j*h/3 + w/6)
-- >>>    let actions =
-- >>>            [ cairoScope $ do
-- >>>                grid 0 0
-- >>>                for_ (take 10 [0..]) $ \i -> do
-- >>>                    let scaleFactor = lerpID (0,9) (0.1, 1) i
-- >>>                    sketch (transform (scaleAround center scaleFactor) ellipse)
-- >>>                    C.stroke
-- >>>            , cairoScope $ do
-- >>>                grid 1 0
-- >>>                for_ (take 10 [0..]) $ \i -> do
-- >>>                    let scaleFactor = lerpID (0,9) (0.1, 1) i
-- >>>                    sketch (transform (scaleAround' center scaleFactor 1) ellipse)
-- >>>                    C.stroke
-- >>>            , cairoScope $ do
-- >>>                grid 2 0
-- >>>                for_ [0..9] $ \i -> do
-- >>>                    let scaleFactor1 = lerp (0, 9) (1, 0.1) (fromIntegral i)
-- >>>                        scaleFactor2 = lerp (0, 9) (0.1, 1) (fromIntegral i)
-- >>>                    sketch (transform (scaleAround' center scaleFactor1 scaleFactor2) ellipse)
-- >>>                    C.stroke
-- >>>            , cairoScope $ do
-- >>>                grid 0 1
-- >>>                for_ (take 10 [0..]) $ \i -> do
-- >>>                    let scaleX = scaleAround' center (lerpID (0,9) (0.5,1) (fromIntegral i)) 1
-- >>>                        scaleY = scaleAround' center 1 (lerpID (0,9) (0.1,1) (fromIntegral i))
-- >>>                    sketch (transform (scaleX <> scaleY) ellipse)
-- >>>                    C.stroke
-- >>>            , cairoScope $ do
-- >>>                grid 1 1
-- >>>                for_ (take 10 [0..]) $ \i -> do
-- >>>                    let angle = deg (lerpID (0,9) (0, 90) i)
-- >>>                    sketch (transform (rotateAround center angle <> scaleAround' center 1 0.5) ellipse)
-- >>>                    C.stroke
-- >>>            , cairoScope $ do
-- >>>                grid 2 1
-- >>>                for_ (take 19 [0..]) $ \i -> do
-- >>>                    let angle = deg (lerpID (0,19) (0, 180) i)
-- >>>                    sketch (transform (rotateAround center angle <> scaleAround' center 1 0.5) ellipse)
-- >>>                    C.stroke
-- >>>            , cairoScope $ do
-- >>>                grid 0 2
-- >>>                for_ (take 9 [0..]) $ \i -> do
-- >>>                    let scaleFactor = lerpID (0,9) (1,0.1) i
-- >>>                        angle = deg (lerpID (0,9) (90,0) i)
-- >>>                    sketch (transform (rotateAround center angle <> scaleAround' center 1 scaleFactor) ellipse)
-- >>>                    C.stroke
-- >>>            , cairoScope $ do
-- >>>                grid 1 2
-- >>>                for_ (take 9 [0..]) $ \i -> do
-- >>>                    let scaleFactor = lerpID (0,9) (1,0.1) i
-- >>>                        angle = deg (lerpID (0,9) (0,90) i)
-- >>>                    sketch (transform (scaleAround center scaleFactor <> rotateAround center angle <> scaleAround' center 1 scaleFactor) ellipse)
-- >>>                    C.stroke
-- >>>            , cairoScope $ do
-- >>>                grid 2 2
-- >>>                for_ (take 9 [0..]) $ \i -> do
-- >>>                    let BoundingBox topLeft _ = boundingBox ellipse
-- >>>                        scaleFactor = lerpID (0,9) (1,0.1) i
-- >>>                        angle = deg (lerpID (0,9) (0,90) i)
-- >>>                    sketch (transform (rotateAround center angle <> scaleAround (0.5 *. (topLeft -. center)) scaleFactor) ellipse)
-- >>>                    C.stroke
-- >>>            ]
-- >>>    for_ (zip [0..] actions) $ \(i, action) -> do
-- >>>        setColor (mathematica97 i)
-- >>>        action
-- :}
-- Generated file: size 42KB, crc32: 0xb3cabf7b
newtype Ellipse = Ellipse Transformation
    deriving (Show)

instance NFData Ellipse where rnf _ = ()

-- | Unit circle
instance Default Ellipse where def = Ellipse mempty



-- | <<docs/haddock/Geometry/Core/bounding_box_ellipse.svg>>
--
-- === __(image code)__
-- >>> :{
-- >>> haddockRender "Geometry/Core/bounding_box_ellipse.svg" 300 300 $ do
-- >>>     let (w,h) = (300,300)
-- >>>         radius = w/6*0.9
-- >>>         ellipse = toEllipse (Circle zero radius)
-- >>>         grid i j = C.translate (fromIntegral i*w/3 + w/6) (fromIntegral j*h/3 + w/6)
-- >>>     let paintWithBB i j geo = cairoScope $ do
-- >>>             grid i j
-- >>>             setColor (mathematica97 (i*3+j))
-- >>>             cairoScope $ sketch geo >> C.stroke
-- >>>             cairoScope $ C.setDash [1,1.5] 0 >> sketch (boundingBox geo) >> C.stroke
-- >>>     paintWithBB 0 0 (transform (scale 0.9) ellipse)
-- >>>     paintWithBB 1 0 (transform (scale 0.75) ellipse)
-- >>>     paintWithBB 2 0 (transform (scale 0.5) ellipse)
-- >>>     paintWithBB 0 1 (transform (scale' 0.5 1) ellipse)
-- >>>     paintWithBB 1 1 (transform (scale' 1 0.5) ellipse)
-- >>>     paintWithBB 2 1 (transform (rotate (deg 30) <> scale' 0.5 1) ellipse)
-- >>>     paintWithBB 0 2 (transform (shear 0.3 0 <> scale' 0.5 1) ellipse)
-- >>>     paintWithBB 1 2 (transform (shear 0 0.3 <> scale' 1 0.5) ellipse)
-- >>>     paintWithBB 2 2 (transform (scale' 1 0.5 <> rotate (deg 45) <> shear 0 1 <> scale' 1 0.5) ellipse)
-- :}
-- Generated file: size 9KB, crc32: 0xac92bb50
instance HasBoundingBox Ellipse where
    boundingBox (Ellipse (Transformation (Mat2 a11 a12 a21 a22) (Vec2 b1 b2))) =
        let -- https://tavianator.com/2014/ellipsoid_bounding_boxes.html
            x_plus  = b1 + sqrt (a11^2+a12^2)
            x_minus = b1 - sqrt (a11^2+a12^2)
            y_plus  = b2 + sqrt (a21^2+a22^2)
            y_minus = b2 - sqrt (a21^2+a22^2)
        in boundingBox (Vec2 x_plus y_plus, Vec2 x_minus y_minus)

instance Transform Ellipse where
    transform t (Ellipse t') = Ellipse (t <> t')

-- | Total length of a 'Polyline'.
--
-- >>> polylineLength (Polyline [zero, Vec2 123.4 0])
-- 123.4
polylineLength :: Polyline -> Double
polylineLength = foldl' (+) 0 . map lineLength . polylineEdges

-- | All lines composing a 'Polyline' (in order).
--
-- >>> polylineEdges (Polyline [zero, Vec2 50 50, Vec2 100 0])
-- [Line (Vec2 0.0 0.0) (Vec2 50.0 50.0),Line (Vec2 50.0 50.0) (Vec2 100.0 0.0)]
polylineEdges :: Polyline -> [Line]
polylineEdges (Polyline points) = zipWith Line points (tail points)

-- | Ray-casting algorithm. Counts how many times a ray coming from infinity
-- intersects the edges of an object.
--
-- The most basic use case is 'pointInPolygon', but it can also be used to find
-- out whether something is inside more complicated objects, such as nested
-- polygons (polygons with holes).
countEdgeTraversals
    :: Foldable list
    => Vec2      -- ^ Point to check
    -> list Line -- ^ Geometry. Each segment must form a closed trajectory (or the ray may escape without registering).
    -> Int       -- ^ Number of edges crossed
countEdgeTraversals subjectPoint edges'
    | overlappingBoundingBoxes subjectPoint edgesBB = length intersections
    | otherwise = 0

  where
    edges = toList edges'
    edgesBB@(BoundingBox (Vec2 leftmostX _) _) = boundingBox edges

    -- The test ray starts beyond the geometry, and ends at the point to be tested.
    --
    -- This ray is numerically sensitive, because exactly crossing a corner of the
    -- polygon might count as 0, 1 or 2 edges traversed. For this reason, we
    -- subtract 1 from the y coordinate as well to get a bit of an odd angle,
    -- greatly reducing the chance of exactly hitting a corner on the way.
    testRay = Line (Vec2 (leftmostX - 1) (pointY - 1)) subjectPoint
    Vec2 _ pointY = subjectPoint

    intersections = filter (\edge ->
        case intersectionLL testRay edge of
            IntersectionReal _ -> True
            _other -> False)
        edges

-- | Is the point inside the polygon?
--
-- <<docs/haddock/Geometry/Core/point_in_polygon.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/point_in_polygon.svg" 180 180 $ do
--     let square = boundingBoxPolygon (shrinkBoundingBox 40 [zero, Vec2 180 180])
--         points = subdivideLine 11 (Line (Vec2 20 120) (Vec2 160 60))
--     C.setLineWidth 2
--     sketch square
--     C.stroke
--     setColor (mathematica97 1)
--     for_ points $ \point -> do
--         sketch (Circle point 4)
--         if pointInPolygon point square then C.fill else C.stroke
-- :}
-- Generated file: size 5KB, crc32: 0xabb5dfc
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

-- | Average of polygon vertices. Note that this is not the same as
-- 'polygonCentroid', which is much less influenced by clustered corners.
--
-- <<docs/haddock/Geometry/Core/polygon_average.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/polygon_average.svg" 100 100 $ do
--     let polygon = Polygon [Vec2 10 10, Vec2 10 90, Vec2 20 70, Vec2 40 60, Vec2 30 40, Vec2 90 90, Vec2 80 20]
--         averate = polygonAverage polygon
--     sketch polygon
--     C.stroke
--     setColor (mathematica97 1)
--     sketch (Circle averate 5)
--     sketch (Cross averate 5)
--     C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0xcf7a8233
polygonAverage :: Polygon -> Vec2
polygonAverage (Polygon corners)
  = let (num, total) = foldl' (\(!n, !vec) corner -> (n+1, vec +. corner)) (0, Vec2 0 0) corners
    in (1/num) *. total

-- | The centroid or geometric center of mass of a polygon.
--
-- <<docs/haddock/Geometry/Core/polygon_centroid.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/polygon_centroid.svg" 100 100 $ do
--     let polygon = Polygon [Vec2 10 10, Vec2 10 90, Vec2 20 70, Vec2 40 60, Vec2 30 40, Vec2 90 90, Vec2 80 20]
--         centroid = polygonCentroid polygon
--     sketch polygon
--     C.stroke
--     setColor (mathematica97 1)
--     sketch (Circle centroid 5)
--     sketch (Cross centroid 5)
--     C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0x4453ccc1
polygonCentroid :: Polygon -> Vec2
polygonCentroid poly@(Polygon ps) = weight *. vsum (zipWith (\p q -> cross p q *. (p +. q)) ps (tail (cycle ps)))
  where
    totalArea = signedPolygonArea poly
    weight = 1 / (6 * totalArea)

-- | Sum of all edge lengths.
polygonCircumference :: Polygon -> Double
polygonCircumference = foldl' (\acc edge -> acc + lineLength edge) 0 . polygonEdges

-- | Move all edges of a polygon outwards by the specified amount. Negative values
-- shrink instead (or use 'shrinkPolygon').
--
-- <<docs/haddock/Geometry/Core/grow_polygon.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/grow_polygon.svg" 160 230 $ do
--     let polygon = transform (scale 2) $ Polygon [Vec2 20 40, Vec2 20 80, Vec2 40 60, Vec2 60 80, Vec2 60 40, Vec2 40 20]
--     for_ [0,5..25] $ \offset -> cairoScope $ do
--         when (offset == 0) (C.setLineWidth 3)
--         setColor (icefire (Numerics.Interpolation.lerp (0,25) (0.5, 1) (fromIntegral offset)))
--         sketch (growPolygon (fromIntegral offset) polygon)
--         C.stroke
-- :}
-- Generated file: size 4KB, crc32: 0xd6cd58c1
growPolygon :: Double -> Polygon -> Polygon
growPolygon offset polygon =
    let oldEdges = polygonEdges polygon

        -- Alg idea:
        -- Compare edge with expanded/shrunken edge. Ears have reversed direction to before,
        -- so we drop all the ear edges, and recompute the intersections of the remaining edges.

        grownEdges =
            let offsetOriented = case polygonOrientation polygon of
                    PolygonNegative -> offset
                    PolygonPositive -> -offset
            in map (moveLinePerpendicular offsetOriented) oldEdges

        newCorners = rotateListRight1 (adjacentIntersections grownEdges)
                     -- We need to rotate the list by one, otherwise the edges
                     -- will be misaligned by one, and we’ll be comparing an
                     -- edge to a resized _other_ edge in the next step. Bit hacky,
                     -- refactorings welcome :-)

        oldAndNewEdges = zipWith3
            (\oldEdge newCorner1 newCorner2 -> (oldEdge, Line newCorner1 newCorner2))
            oldEdges
            newCorners
            (tail (cycle newCorners))

        sameDirection v w = dotProduct (vectorOf v) (vectorOf w) >= 0

        earsClipped = do
            (oldEdge, newEdge) <- oldAndNewEdges
            -- Ears have flipped directions so we can filter them out
            guard (sameDirection oldEdge newEdge)
            pure newEdge

        earsClippedCorners = adjacentIntersections earsClipped

    in Polygon earsClippedCorners

-- | Convenience version of 'growPolygon' for negative deltas.
--
-- <<docs/haddock/Geometry/Core/shrink_polygon.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/shrink_polygon.svg" 160 230 $ do
--     let polygon = transform (scale 2) $ Polygon [Vec2 20 40, Vec2 20 80, Vec2 40 60, Vec2 60 80, Vec2 60 40, Vec2 40 20]
--     for_ [0,5..25] $ \offset -> cairoScope $ do
--         when (offset == 0) (C.setLineWidth 3)
--         setColor (icefire (Numerics.Interpolation.lerp (0,15) (0.5, 0) (fromIntegral offset)))
--         sketch (shrinkPolygon (fromIntegral offset) polygon)
--         C.stroke
-- :}
-- Generated file: size 4KB, crc32: 0xc5085e44
shrinkPolygon :: Double -> Polygon -> Polygon
shrinkPolygon delta = growPolygon (-delta)

rotateListRight1 :: [a] -> [a]
rotateListRight1 [] = []
rotateListRight1 xs = last xs : init xs

moveLinePerpendicular :: Double -> Line -> Line
moveLinePerpendicular offset line =
    let -- | Rotate 90 degrees. Fast special case of 'transform (rotate ('deg' 90))'.
        rot90 :: Vec2 -> Vec2
        rot90 (Vec2 x y) = Vec2 (-y) x
        dir = rot90 (direction line)
    in transform (translate (offset *. dir)) line

-- | Pairwise intersections of lines. Useful to reconstruct a polygon from a list of edges.
adjacentIntersections :: [Line] -> [Vec2]
adjacentIntersections edges = zipWith
    (\edge1@(Line _ fallback1) edge2@(Line fallback2 _) -> case intersectionLL edge1 edge2 of
        IntersectionVirtual p        -> p -- Most common case on growing
        IntersectionReal p           -> p -- Most common case on shrinking
        IntersectionVirtualInsideL p -> p -- Not sure when this might happen, but if it does that’s what it should do :-)
        IntersectionVirtualInsideR p -> p -- Dito
        Parallel                     -> (fallback1 +. fallback2) /. 2 -- Pathological polygon: edge goes back onto itself
        Collinear{}                  -> (fallback1 +. fallback2) /. 2 -- Collinear edges, drop the middle point
    )
    edges
    (tail (cycle edges))

-- | Two-dimensional cross product.
--
-- This is useful to calculate the (signed) area of the parallelogram spanned by
-- two vectors, or to check whether a vector is to the left or right of another
-- vector.
--
-- >>> cross (Vec2 1 0) (Vec2 1 0) -- Colinear
-- 0.0
--
-- >>> cross (Vec2 1 0) (Vec2 1 0.1) -- 2nd vec is in positive (counter-clockwise) direction
-- 0.1
--
-- >>> cross (Vec2 1 0) (Vec2 1 (-0.1)) -- 2nd vec is in negative (clockwise) direction
-- -0.1
cross :: Vec2 -> Vec2 -> Double
cross (Vec2 x1 y1) (Vec2 x2 y2) = det (Mat2 x1 y1 x2 y2)

-- | Determinant a matrix.
det :: Mat2 -> Double
det (Mat2 a11 a12 a21 a22) = a11*a22 - a12*a21

-- | Area of a polygon.
polygonArea :: Polygon -> Double
polygonArea = abs . signedPolygonArea

-- | Area of a polygon. The result’s sign depends on orientation: 'PolygonPositive' 'Polygon's have positive area.
--
-- >>> signedPolygonArea (Polygon [Vec2 0 0, Vec2 10 0, Vec2 10 10, Vec2 0 10])
-- 100.0
--
-- >>> signedPolygonArea (Polygon [Vec2 0 0, Vec2 0 10, Vec2 10 10, Vec2 10 0])
-- -100.0
signedPolygonArea :: Polygon -> Double
signedPolygonArea (Polygon ps)
  = let determinants = zipWith cross ps (tail (cycle ps))
    in sum determinants / 2

-- | Check whether the polygon is convex.
--
-- <<docs/haddock/Geometry/Core/is_convex.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/is_convex.svg" 200 100 $ do
--     let convex = Polygon [Vec2 10 10, Vec2 10 90, Vec2 90 90, Vec2 90 10]
--         concave = Polygon [Vec2 110 10, Vec2 110 90, Vec2 150 50, Vec2 190 90, Vec2 190 10]
--     for_ [convex, concave] $ \polygon -> do
--         if isConvex polygon
--             then setColor (mathematica97 2)
--             else setColor (mathematica97 3)
--         sketch polygon
--         C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0x60a3e9a1
isConvex :: Polygon -> Bool
isConvex (Polygon ps) = allSameSign angleDotProducts
    -- The idea is that a polygon is convex iff all internal angles are in the
    -- same direction. The direction of an angle defined by two vectors shares
    -- its sign with the signed area spanned by those vectors, and the latter is
    -- easy to calculate via a determinant.
  where
    angleDotProducts = zipWith3
        (\p q r -> cross (q -. p) (r -. q) )
        ps
        (tail (cycle ps))
        (tail (tail (cycle ps)))

    allSameSign [] = True
    allSameSign (x:xs) = all (\p -> signum p == signum x) xs

-- | The result has the same length as the input, point in its center, and
-- points to the left (90° turned CCW) relative to the input.
--
-- <<docs/haddock/Geometry/Core/perpendicular_bisector.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/perpendicular_bisector.svg" 200 160 $ do
--     let line = Line (Vec2 20 20) (Vec2 190 90)
--         bisector = perpendicularBisector line
--     C.setLineWidth 2
--     sketch line >> C.stroke
--     sketch bisector >> setColor (mathematica97 1) >> C.stroke
-- :}
-- Generated file: size 2KB, crc32: 0x9940c32d
perpendicularBisector :: Line -> Line
perpendicularBisector (Line start end) =
    let middle = 0.5 *. (end +. start)
    in rotateLine90 (Line middle end)

rotateLine90 :: Line -> Line
rotateLine90 (Line start end) = Line start end'
  where
    end' = rotate90 (end -. start) +. start

rotate90 :: Vec2 -> Vec2
rotate90 (Vec2 x y) = Vec2 (-y) x

-- | Line perpendicular to a given line through a point, starting at the
-- intersection point.
--
-- If the point is on the line directly, fall back to a perpendicular line through
-- the point of half the length of the input.
--
-- This is also known as the vector projection. For vectors \(\mathbf a\) (pointing
-- from the start of the line to \(\mathbf p\)) and \(\mathbf b\) (pointing from
-- the start of the line to its end),
--
-- \[
-- \mathrm{proj}_{\mathbf b}(\mathbf a)
-- = \frac{\mathbf a\cdot\mathbf b}{\mathbf b\cdot\mathbf b}\mathbf b
-- \]
--
-- <<docs/haddock/Geometry/Core/perpendicular_line_through.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/perpendicular_line_through.svg" 170 170 $ do
--     let line = transform (translate (Vec2 20 20))
--                          (Line zero (Vec2 (3*40) (4*20)))
--         points =
--             [ Vec2 20 110 -- above
--             , Vec2 70 90 -- above
--             , Vec2 130 20 -- below
--             , Vec2 110 80 -- directly on
--             , Vec2 130 150 -- beyond
--             ]
--     C.setLineWidth 2
--     sketch line >> C.stroke
--     for_ (zip [1..] points) $ \(i, p) -> do
--         setColor (mathematica97 i)
--         cairoScope $ sketch (perpendicularLineThrough p line) >> C.setDash [3,5] 0 >> C.stroke
--         cairoScope $ sketch (Circle p 5) >> C.fill
-- :}
-- Generated file: size 4KB, crc32: 0xac73c163
perpendicularLineThrough :: Vec2 -> Line -> Line
perpendicularLineThrough p (Line start end) =
    let a = p -. start
        b = end -. start
        proj = (dotProduct a b/dotProduct b b) *. b +. start
    in if normSquare (p -. proj) <= 0.01^2
        then -- p is too close to proj, so a 'Line' does not make
             -- any sense. Fall back to a line of half the input
             -- length perpendicular to the original one.
            let p' = proj +. rotate90 ((end -. start) /. 2)
            in Line proj p'
        else Line proj p

-- | Optical reflection of a ray on a mirror. Note that the outgoing line has
-- reversed direction like light rays would. The second result element is the
-- point of intersection with the mirror, which is not necessarily on the line,
-- and thus returned separately.
--
-- <<docs/haddock/Geometry/Core/reflection.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Core/reflection.svg" 520 300 $ do
--    let mirrorSurface = angledLine (Vec2 10 100) (deg 10) 510
--    cairoScope $ do
--        C.setLineWidth 2
--        setColor (black `withOpacity` 0.5)
--        sketch mirrorSurface
--        C.stroke
--    let angles = [-135,-120.. -10]
--    cairoScope $ do
--        let rayOrigin = Vec2 180 250
--        setColor (hsv 0 1 0.7)
--        sketch (Circle rayOrigin 5)
--        C.stroke
--        for_ angles $ \angleDeg -> do
--            let rayRaw = angledLine rayOrigin (deg angleDeg) 100
--                Just (Line _ reflectedRayEnd, iPoint, _) = reflection rayRaw mirrorSurface
--                ray = Line rayOrigin iPoint
--                ray' = Line iPoint reflectedRayEnd
--            setColor (flare (lerp (minimum angles, maximum angles) (0.2,0.8) angleDeg))
--            sketch ray
--            sketch ray'
--            C.stroke
--    cairoScope $ do
--        let rayOrigin = Vec2 350 30
--        setColor (hsva 180 1 0.7 1)
--        sketch (Circle rayOrigin 5)
--        C.stroke
--        for_ angles $ \angleDeg -> do
--            let rayRaw = angledLine rayOrigin (deg angleDeg) 100
--                Just (Line _ reflectedRayEnd, iPoint, _) = reflection rayRaw mirrorSurface
--                ray = Line rayOrigin iPoint
--                ray' = Line iPoint reflectedRayEnd
--            setColor (crest (lerp (minimum angles, maximum angles) (0,1) angleDeg))
--            sketch ray
--            sketch ray'
--            C.stroke
-- :}
-- Generated file: size 9KB, crc32: 0x76885f25
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
