{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Penrose where

import Prelude hiding (length, flip)
import Geometry

data Face = Face
    { faceType :: FaceType
    , faceP0 :: Vec2
    , faceP1 :: Vec2
    , faceP2 :: Vec2 }
    deriving (Show)

asPolygon :: Face -> Polygon
asPolygon Face{..} = Polygon [faceP0, faceP1, faceP2]
instance Transform Face where
    transform t f@Face{..} = f
        { faceP0 = transform t faceP0
        , faceP1 = transform t faceP1
        , faceP2 = transform t faceP2 }

instance Rotate Face where
    rotateAround pivot theta = move pivot . transform (rotate' theta) . move (negateVec2 pivot)

instance Move Face where
    move (Vec2 x y) = transform (translate' x y)

instance Mirror Face where
    mirrorAlong line face@Face{..} = face
        { faceP0 = mirrorAlong line faceP0
        , faceP1 = mirrorAlong line faceP1
        , faceP2 = mirrorAlong line faceP2
        }

data FaceType = Thin | Thick
    deriving (Eq, Show)

subdivide :: Face -> [Face]
subdivide Face{..} = case faceType of
    Thin ->
        [ Face
            { faceType = Thick
            , faceP0 = faceP0
            , faceP1 = newPoint
            , faceP2 = faceP1 }
        , Face
            { faceType = Thin
            , faceP0 = faceP2
            , faceP1 = faceP0
            , faceP2 = newPoint } ]
      where
        newPoint = faceP2 +. (1/phi-1) *. v
        v = faceP2 -. faceP1
    Thick ->
        [ Face
            { faceType = Thick
            , faceP0 = newPoint2
            , faceP1 = newPoint1
            , faceP2 = faceP0 }
        , Face
            { faceType = Thin
            , faceP0 = faceP1
            , faceP1 = newPoint2
            , faceP2 = newPoint1 }
        , Face
            { faceType = Thick
            , faceP0 = faceP2
            , faceP1 = newPoint2
            , faceP2 = faceP1 } ]
      where
        newPoint1 = faceP1 +. (1/phi-1) *. v1
        v1 = faceP1 -. faceP0
        newPoint2 = faceP2 +. (1/phi-1) *. v2
        v2 = faceP2 -. faceP0

twin :: Face -> Face
twin f@Face{..} = f { faceP1 = mirrorAlong (Line faceP0 faceP2) faceP1 }

-- | Keeps the same shape, but reverses the orientation.
flip :: [Face] -> [Face]
flip = fmap $ \f@Face{..} -> f { faceP0 = faceP2, faceP2 = faceP0 }

inside :: Vec2 -> Face -> Bool
p `inside` Face{..} = s1 == s2 && s1 == s3
  where
    sign (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3) = signum $ (x1-x3)*(y2-y3) - (x2-x3)*(y1-y3)
    s1 = sign p faceP0 faceP1
    s2 = sign p faceP1 faceP2
    s3 = sign p faceP2 faceP0

inscribedPentagons :: Face -> [Polygon]
inscribedPentagons f@Face{..} = case faceType of
    Thin -> [Polygon [p0, p1, p2, p3]]
      where
        center = faceP2 +. a *. (faceP1 -. faceP2)
        v0 = p0 -. center
        p0 = center +. 1/phi *. (faceP1 -. center)
        p1 = center +. rotate theta v0
        p2 = center +. rotate (2*theta) v0
        p3 = center +. 0.5 *. (rotate (2*theta) v0 +. rotate (3*theta) v0)

    Thick -> [pentagon1, pentagon2]
      where
        pentagon1 = Polygon [p0, p1, p2, p3]
          where
            center = faceP2 +. a *. (faceP0 -. faceP2)
            v1 = p1 -. center
            p0 = center +. 0.5 *. (v1 +. rotate (-theta) v1)
            p1 = faceP2 +. a *. (faceP1 -. faceP2)
            p2 = center +. rotate theta v1
            p3 = center +. rotate (2*theta) v1
        pentagon2 = Polygon [p0, p1, p2, p3]
          where
            center = faceP1 +. a *. (faceP0 -. faceP1)
            v0 = p0 -. center
            p0 = center +. 1/phi *. (faceP0 -. center)
            p1 = center +. rotate theta v0
            p2 = center +. rotate (2*theta) v0
            p3 = center +. 0.5 *. (rotate (2*theta) v0 +. rotate (3*theta) v0)
  where
    a = 1 - 1/phi
    theta = case polygonOrientation (asPolygon f) of
        PolygonPositive -> 2*pi/5
        PolygonNegative -> -2*pi/5
    rotate alpha = transform (rotate' (rad alpha))

phi :: Double
phi = (1+sqrt 5)/2

thinFaceBase :: [Face]
thinFaceBase = [baseFace, twin baseFace]
  where
    baseFace = Face
        { faceType = Thin
        , faceP0 = rotate (rad (-pi/10)) $ Vec2 1 0
        , faceP1 = Vec2 0 0
        , faceP2 = rotate (rad (pi/10)) $ Vec2 1 0
        }

thickFaceBase :: [Face]
thickFaceBase = [baseFace, twin baseFace]
  where
    baseFace = Face
        { faceType = Thick
        , faceP0 = Vec2 phi 0
        , faceP1 = rotate (rad (-pi/5)) $ Vec2 1 0
        , faceP2 = Vec2 0 0
        }

decagonRose :: Vec2 -> Double -> [Face]
decagonRose center r =
    let outer = move (Vec2 phi 0) . rotate (rad (7*pi/10)) $ thinFaceBase
    in  star2 center r ++ ((rotateAround center . rad . (2*pi/5 *) <$> [0..4]) <*> scaleTo center r outer)

star1 :: Vec2 -> Double -> [Face]
star1 center r =
    let inner = flip thickFaceBase
    in  (rotateAround center . rad . (2*pi/5 *) <$> [0..4]) <*> scaleTo center r inner

star2 :: Vec2 -> Double -> [Face]
star2 center r = scaleTo center r ((rotate . rad . (2*pi/5 *) <$> [0..4]) <*> thickFaceBase)

asymmetricDecagon :: Vec2 -> Double -> [Face]
asymmetricDecagon center r = scaleTo center r $ concat
    [ offAxisFaces, mirrorY offAxisFaces, onAxisFaces ]
  where
    origin = Vec2 (-phi) 0
    edge = Vec2 1 0
    f1 = move origin $ rotate (rad (pi/5)) $ flip thickFaceBase
    f2 = move (origin +. edge) $ rotate (rad (3*pi/10)) $ flip thinFaceBase
    f3 = mirrorAlong (angledLine origin (rad (pi/5)) (Distance 1)) f2
    f4 = move (origin +. edge) $ flip thickFaceBase
    f5 = move edge $ rotate (rad (3*pi/5)) thickFaceBase
    f6 = flip $ move (negateVec2 origin) $ rotate (rad (pi/2)) $ move (rotate (rad (9*pi/10)) edge) thinFaceBase
    offAxisFaces = concat [f1, f2, f3, f5]
    onAxisFaces = concat [f4, f6]


scaleTo :: Vec2 -> Double -> [Face] -> [Face]
scaleTo center size = fmap (move center . transform (scale' (size/phi) (size/phi)))
