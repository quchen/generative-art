{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Penrose where

import Data.Foldable (find, for_, traverse_)
import Prelude hiding (length, flip)
import Data.List (partition)
import Geometry

data Face = Face
    { faceType :: FaceType
    , faceP0 :: Vec2
    , faceP1 :: Vec2
    , faceP2 :: Vec2
    , faceOrientation :: FaceOrientation }
    deriving (Show)

data FaceType = Thin | Thick
    deriving (Eq, Show)

data FaceOrientation = Positive | Negative
    deriving (Eq, Show)

rotateFace :: Vec2 -> Double -> Face -> Face
rotateFace pivot theta face@Face{..} = face
    { faceP0 = faceP0'
    , faceP1 = faceP1'
    , faceP2 = faceP2' }
  where
    faceP0' = rotateAroundPivot faceP0
    faceP1' = rotateAroundPivot faceP1
    faceP2' = rotateAroundPivot faceP2
    rotateAroundPivot p = translate (rotate theta (p -. pivot)) pivot

translateFace :: Vec2 -> Face -> Face
translateFace v face@Face{..} = face
    { faceP0 = translate v faceP0
    , faceP1 = translate v faceP1
    , faceP2 = translate v faceP2 }

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
        newPoint = translate (v `vtimes` (-1+1/phi)) faceP2
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
        newPoint1 = translate (v1 `vtimes` (-1+1/phi)) faceP1
        v1 = faceP1 -. faceP0
        newPoint2 = translate (v2 `vtimes` (-1+1/phi)) faceP2
        v2 = faceP2 .- faceP0

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

inscribedPentagons :: Face -> [[Vec2]]
inscribedPentagons Face{..} = case faceType of
    Thin -> [[p0, p1, p2, p3]]
      where
        center = translate ((faceP1 -. faceP2) `vtimes` a) faceP2
        v0 = p0 -. center
        p0 = translate ((faceP1 -. center) `vtimes` (1/phi)) center
        p1 = translate (rotate theta v0) center
        p2 = translate (rotate (2*theta) v0) center
        p3 = translate ((rotate (2*theta) v0 `vplus` rotate (3*theta) v0) `vtimes` 0.5) center

    Thick -> [pentagon1, pentagon2]
      where
        pentagon1 = [p0, p1, p2, p3]
          where
            center = translate ((faceP0 -. faceP2) `vtimes` a) faceP2
            v1 = p1 -. center
            p0 = translate ((v1 `vplus` rotate (-theta) v1) `vtimes` 0.5) center
            p1 = translate ((faceP1 -. faceP2) `vtimes` a) faceP2
            p2 = translate (rotate theta v1) center
            p3 = translate (rotate (2*theta) v1) center
        pentagon2 = [p0, p1, p2, p3]
          where
            center = translate ((faceP0 -. faceP1) `vtimes` a) faceP1
            v0 = p0 -. center
            p0 = translate ((faceP0 -. center) `vtimes` (1/phi)) center
            p1 = translate (rotate theta v0) center
            p2 = translate (rotate (2*theta) v0) center
            p3 = translate ((rotate (2*theta) v0 `vplus` rotate (3*theta) v0) `vtimes` 0.5) center
  where
    a = 1 - 1/phi
    theta = case faceOrientation of
        Positive -> 2*pi/5
        Negative -> -2*pi/5

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
    in (rotateFace center . (2*pi/5*) <$> [0..4]) <*> initialFaces
