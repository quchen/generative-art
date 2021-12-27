{-# LANGUAGE BangPatterns #-}

-- | Predefined shapes.
module Geometry.Shapes (
      haskellLogo
    , regularPolygon
    , spiralPolygon
) where



import Data.List

import Geometry.Core



-- | Haskell logo, in the order @[left, lambda, upper dash, lower dash]@.
--
-- The logo is scaled so that the height of the logo is @1@.
--
-- Each polygon starts at the bottom/left corner in screen coordinates (i.e.
-- @(0,0)@ is at the top right, x extends to the right, and y extends
-- downwards).
--
-- The orientation is in mathematically positive direction, i.e. clockwise in
-- screen coordinates.
haskellLogo :: [Polygon]
haskellLogo = rescaleNormalizePolygons haskellLogoRaw

-- | Rescale so that in drawing coordinates, the top/left is at the origin, and
-- the height extents is 1.
rescaleNormalizePolygons :: [Polygon] -> [Polygon]
rescaleNormalizePolygons polygons
  = let (c:orners) = [ corner | Polygon corners <- polygons
                              , corner <- corners ]
        (minX, minY, maxY)
          = foldl' (\(!minX', !minY', !maxY') (Vec2 x y)
                        -> (min x minX', min y minY', max y maxY'))
                   (let Vec2 x y = c in (x, y, y))
                   orners
        scaleFactor = 1 / (maxY - minY)
        transformation = scaleT scaleFactor scaleFactor <> translateT (Vec2 (- minX) (- minY))
    in transform transformation polygons

haskellLogoRaw :: [Polygon]
haskellLogoRaw = [left, lambda, upper, lower]
  where
    left   = Polygon [Vec2 0 340.15625, Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625]
    lambda = Polygon [Vec2 113.386719 340.15625, Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625]
    upper  = Polygon [Vec2 330.710938 155.90625, Vec2 292.914062 99.214844, Vec2 481.890625 99.210938, Vec2 481.890625 155.90625]
    lower  = Polygon [Vec2 387.402344 240.945312, Vec2 349.609375 184.253906, Vec2 481.890625 184.25, Vec2 481.890625 240.945312]

-- | Rectangular spiral.
spiralPolygon
    :: Int -- ^ Winding number
    -> Double -- ^ Width
    -> Polygon
spiralPolygon n width = Polygon (scanl (+.) (Vec2 0 0) relativeSpiral)
  where
    instructions = concat [ zip [1..n] (repeat turnLeft)
                          , [(1, turnLeft)]
                          , [(n-1, turnRight)]
                          , zip [n-3, n-4 .. 1] (repeat turnRight)
                          ]
    relativeSpiral = go instructions (Vec2 1 0)
      where
        go [] _dir = []
        go ((len, makeCorner) : rest) dir = width*fromIntegral len *. dir : go rest (makeCorner dir)
    turnLeft  (Vec2 x y) = Vec2   y  (-x)
    turnRight (Vec2 x y) = Vec2 (-y)   x

-- Regular n-gon with radius 1, oriented in mathematically positive direction,
-- and starting with the first corner on the positive x axis.
regularPolygon :: Int -> Polygon
regularPolygon n = Polygon
    [ rotate (rad d) (Vec2 1 0)
        | d <- take n [0, 2*pi/fromIntegral n ..] ]
