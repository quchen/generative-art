-- | Predefined shapes.
module Geometry.Shapes (
      haskellLogo
    , regularPolygon
    , spiralPolygon
) where



import Geometry.Core



-- $setup
-- >>> import Draw as D
-- >>> import qualified Graphics.Rendering.Cairo as C



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
--
-- <<docs/haddock/Geometry/Shapes.hs/haskell_logo.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Shapes.hs/haskell_logo.svg" 130 100 $ do
--     coordinateSystem CairoStandard_ZeroTopLeft_XRight_YDown
--     for_ haskellLogo $ \polygon -> do
--         sketch (transform (translate (Vec2 10 10) <> scale 80) polygon)
--         C.stroke
-- :}
-- docs/haddock/Geometry/Shapes.hs/haskell_logo.svg
--
-- >>> all (\polygon -> polygonOrientation polygon == PolygonPositive) haskellLogo
-- True
haskellLogo :: [Polygon]
haskellLogo = rescaleNormalizePolygons haskellLogoRaw

-- | Rescale so that in drawing coordinates, the top/left is at the origin, and
-- the height extents is 1.
rescaleNormalizePolygons :: [Polygon] -> [Polygon]
rescaleNormalizePolygons polygons =
    let BoundingBox (Vec2 minX minY) (Vec2 _maxX maxY) = boundingBox polygons
        scaleFactor = 1 / (maxY - minY)
        transformation = scale scaleFactor <> translate (Vec2 (- minX) (- minY))
    in transform transformation polygons

haskellLogoRaw :: [Polygon]
haskellLogoRaw = [left, lambda, upper, lower]
  where
    left   = Polygon [Vec2 0 340.15625, Vec2 113.386719 170.078125, Vec2 0 0, Vec2 85.039062 0, Vec2 198.425781 170.078125, Vec2 85.039062 340.15625]
    lambda = Polygon [Vec2 113.386719 340.15625, Vec2 226.773438 170.078125, Vec2 113.386719 0, Vec2 198.425781 0, Vec2 425.195312 340.15625, Vec2 340.15625 340.15625, Vec2 269.292969 233.859375, Vec2 198.425781 340.15625]
    upper  = Polygon [Vec2 330.710938 155.90625, Vec2 292.914062 99.214844, Vec2 481.890625 99.210938, Vec2 481.890625 155.90625]
    lower  = Polygon [Vec2 387.402344 240.945312, Vec2 349.609375 184.253906, Vec2 481.890625 184.25, Vec2 481.890625 240.945312]

-- | Rectangular spiral. Useful as an example for very much non-convex polygons. 'PolygonPositive' orientation.
--
-- <<docs/haddock/Geometry/Shapes.hs/spiral_polygon.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Shapes.hs/spiral_polygon.svg" 100 100 $ do
--     coordinateSystem (MathStandard_ZeroCenter_XRight_YUp 100 100)
--     let polygon = spiralPolygon 8 10
--     sketch polygon
--     C.stroke
-- :}
-- docs/haddock/Geometry/Shapes.hs/spiral_polygon.svg
--
-- >>> polygonOrientation (spiralPolygon 8 10) == PolygonPositive
-- True
spiralPolygon
    :: Int -- ^ Winding number
    -> Double -- ^ Width
    -> Polygon
spiralPolygon n width = Polygon (reverse (scanl (+.) (Vec2 0 0) relativeSpiral))
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

-- | Regular n-gon with radius 1, oriented 'PolygonPositive',
-- and starting with the first corner on the positive x axis.
--
-- <<docs/haddock/Geometry/Shapes.hs/regular_pentagon.svg>>
--
-- === __(image code)__
-- >>> :{
-- haddockRender "Geometry/Shapes.hs/regular_pentagon.svg" 100 100 $ do
--     let polygon = transform (translate (Vec2 50 50) <> scale 45) (regularPolygon 5)
--     sketch polygon
--     C.stroke
-- :}
-- docs/haddock/Geometry/Shapes.hs/regular_pentagon.svg
--
-- >>> polygonOrientation (regularPolygon 5) == PolygonPositive
-- True
regularPolygon :: Int -> Polygon
regularPolygon n =
    let angleStepSize = 360/fromIntegral n
    in Polygon [polar (deg angle) 1 | angle <- takeWhile (<360) (iterate (+angleStepSize) 0)]
