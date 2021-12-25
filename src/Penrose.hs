{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Penrose where

import Prelude hiding (length, flip)
import Geometry

data Face = Face
    { faceType :: FaceType
    , faceP0 :: Vec2
    , faceP1 :: Vec2
    , faceP2 :: Vec2
    , faceOrientation :: FaceOrientation }
    deriving (Show)

instance Transform Face where
    transform t f@Face{..} = f
        { faceP0 = transform t faceP0
        , faceP1 = transform t faceP1
        , faceP2 = transform t faceP2 }

instance Rotate Face where
    rotateAround pivot theta = move pivot . transform (rotate' theta) . move (negateVec2 pivot)

instance Move Face where
    move (Vec2 x y) = transform (translate' x y)

data FaceType = Thin | Thick
    deriving (Eq, Show)

data FaceOrientation = Positive | Negative
    deriving (Eq, Show)

subdivide :: Face -> [Face]
subdivide Face{..} = case faceType of
    Thin ->
        [ Face
            { faceType = Thick
            , faceOrientation = flip faceOrientation
            , faceP0 = faceP0
            , faceP1 = newPoint
            , faceP2 = faceP1 }
        , Face
            { faceType = Thin
            , faceOrientation = faceOrientation
            , faceP0 = faceP2
            , faceP1 = faceP0
            , faceP2 = newPoint } ]
      where
        newPoint = faceP2 +. (1/phi-1) *. v
        v = faceP1 -. faceP2
    Thick ->
        [ Face
            { faceType = Thick
            , faceOrientation = flip faceOrientation
            , faceP0 = newPoint2
            , faceP1 = newPoint1
            , faceP2 = faceP0 }
        , Face
            { faceType = Thin
            , faceOrientation = faceOrientation
            , faceP0 = faceP1
            , faceP1 = newPoint2
            , faceP2 = newPoint1 }
        , Face
            { faceType = Thick
            , faceOrientation = faceOrientation
            , faceP0 = faceP2
            , faceP1 = newPoint2
            , faceP2 = faceP1 } ]
      where
        newPoint1 = faceP1 +. (1/phi-1) *. v1
        v1 = faceP1 -. faceP0
        newPoint2 = faceP2 +. (1/phi-1) *. v2
        v2 = faceP2 -. faceP0

flip :: FaceOrientation -> FaceOrientation
flip = \case
    Positive -> Negative
    Negative -> Positive

inside :: Vec2 -> Face -> Bool
p `inside` Face{..} = s1 == s2 && s1 == s3
  where
    sign (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3) = signum $ (x1-x3)*(y2-y3) - (x2-x3)*(y1-y3)
    s1 = sign p faceP0 faceP1
    s2 = sign p faceP1 faceP2
    s3 = sign p faceP2 faceP0

inscribedPentagons :: Face -> [Polygon]
inscribedPentagons Face{..} = case faceType of
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
    theta = case faceOrientation of
        Positive -> 2*pi/5
        Negative -> -2*pi/5
    rotate alpha = transform (rotate' (rad alpha))

phi :: Double
phi = (1+sqrt 5)/2

decagonRose :: Vec2 -> Double -> [Face]
decagonRose center@(Vec2 x y) r =
    let initialFaces =
            [ Face
                { faceType = Thick
                , faceOrientation = Positive
                , faceP2 = Vec2 x y
                , faceP1 = Vec2 (x + r/2) (y - r/2 * tan (pi/5))
                , faceP0 = Vec2 (x + r) y }
            , Face
                { faceType = Thick
                , faceOrientation = Negative
                , faceP2 = Vec2 x y
                , faceP1 = Vec2 (x + r/2) (y + r/2 * tan (pi/5))
                , faceP0 = Vec2 (x + r) y }
            , Face
                { faceType = Thin
                , faceOrientation = Positive
                , faceP2 = Vec2 (x + r/2) (y + r/2 * tan (pi/5))
                , faceP1 = Vec2 (x + r) y
                , faceP0 = Vec2 (x + r * cos (pi/5)) (y + r * sin (pi/5)) }
            , Face
                { faceType = Thin
                , faceOrientation = Negative
                , faceP2 = Vec2 (x + r/2) (y - r/2 * tan (pi/5))
                , faceP1 = Vec2 (x + r) y
                , faceP0 = Vec2 (x + r * cos (pi/5)) (y - r * sin (pi/5)) } ]
    in (rotateAround center . rad . (2*pi/5 *) <$> [0..4]) <*> initialFaces
