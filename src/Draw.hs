{-# LANGUAGE RecordWildCards #-}

module Draw (
    -- * SVG and PNG file handling
      withSurfaceAuto
    , withSurface
    , OutputFormat(..)
    , fromExtension

    -- * Colors
    , module Draw.Color
    , module Draw.Color.Schemes.Discrete
    , module Draw.Color.Schemes.Continuous

    -- * Drawing presets
    , moveToVec
    , lineToVec
    , curveToVec
    , lineSketch
    , bezierSketch
    , ArrowSpec(..)
    , arrowSketch
    , circleSketch
    , crossSketch
    , arcSketch
    , arcSketchNegative
    , polygonSketch
    , pathSketch
    , boundingBoxSketch

    -- * Orientation helpers
    , cartesianCoordinateSystem
    , radialCoordinateSystem

    -- * Canvas utilities
    , getSurfaceDimensions

    -- * Temporary Cairo modifications
    , withOperator
    , cairoScope
    , grouped

    -- * Text
    , showTextAligned
    , HAlign(..)
    , VAlign(..)

    -- * Convenience
    , module Data.Default.Class
    , module Data.Foldable
) where



import           Control.Monad
import           Data.Default.Class
import           Data.Foldable
import           Data.List
import           Graphics.Rendering.Cairo as Cairo hiding (x, y)

import Draw.Color
import Draw.Color.Schemes.Discrete
import Draw.Color.Schemes.Continuous
import Geometry

-- | Renders the drawing as PNG or SVG, depending on the file extension. See 'fromExtension'.
withSurfaceAuto :: FilePath -> Int -> Int -> (Surface -> IO a) -> IO a
withSurfaceAuto filePath = withSurface (fromExtension filePath) filePath

-- | Renders the drawing as PNG or SVG
withSurface
    :: OutputFormat      -- ^ Output format
    -> FilePath          -- ^ Output file name
    -> Int               -- ^ Canvas width
    -> Int               -- ^ Canvas height
    -> (Surface -> IO a) -- ^ Drawing action, see 'Cairo.renderWith'
    -> IO a
withSurface PNG file w h action = withImageSurface FormatARGB32 w h $ \surface -> do
    result <- action surface
    surfaceWriteToPNG surface file
    pure result
withSurface SVG file w h draw = withSVGSurface file (fromIntegral w) (fromIntegral h) draw

data OutputFormat = PNG | SVG

-- | Auto-detects the 'OutputFormat' based on the file extension.
fromExtension :: String -> OutputFormat
fromExtension filePath
    | ".png" `isSuffixOf` filePath = PNG
    | ".svg" `isSuffixOf` filePath = SVG
    | otherwise = error ("Unknown file extension: " <> filePath <> ", expecting .png or .svg")

-- | 'Vec2'-friendly version of Cairo’s 'moveTo'.
moveToVec :: Vec2 -> Render ()
moveToVec (Vec2 x y) = moveTo x y

-- | 'Vec2'-friendly version of Cairo’s 'lineTo'.
lineToVec :: Vec2 -> Render ()
lineToVec (Vec2 x y) = lineTo x y

-- | Paint a Cairo curve, which is a Bezier curve where the initial point is
-- implicitly given by where the current draw position is.
curveToVec :: Vec2 -> Vec2 -> Vec2 -> Render ()
curveToVec (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3) = curveTo x1 y1 x2 y2 x3 y3

-- | Convenience function to sketch a 'Line'.
lineSketch :: Line -> Render ()
lineSketch (Line start end) = do
    moveToVec start
    lineToVec end

-- | Sketch a curve consisting out of multiple Bezier segments.
bezierSketch :: Foldable f => f Bezier -> Render ()
bezierSketch = go . toList
  where
    go [] = pure ()
    go (ps@(Bezier start _ _ _ : _)) = do
        moveToVec start
        for_ ps $ \(Bezier _ p1 p2 end) -> curveToVec p1 p2 end

data ArrowSpec = ArrowSpec
    { arrowheadRelPos    :: Double -- ^ Relative position of the arrow head, from 0 (start) to 1 (end). 0.5 paints the arrow in the center.
    , arrowheadSize      :: Double -- ^ Length of each of the sides of the arrow head.
    , arrowDrawBody      :: Bool   -- ^ Draw the arrow’s main body line ('True'), or just the tip ('False')?
    , arrowheadAngle     :: Angle  -- ^ How pointy should the arrow be? 10° is very pointy, 80° very blunt.
    , arrowheadDrawRight :: Bool   -- ^ Draw the left part of the arrow head?
    , arrowheadDrawLeft  :: Bool   -- ^ Draw the right part of the arrow head?
    }

instance Default ArrowSpec where
    def = ArrowSpec
        { arrowheadRelPos    = 1
        , arrowheadSize      = 10
        , arrowDrawBody      = True
        , arrowheadAngle     = rad 0.5
        , arrowheadDrawRight = True
        , arrowheadDrawLeft  = True
        }

-- | Sketch an arrow shape based on the 'ArrowSpec'.
arrowSketch :: Line -> ArrowSpec -> Render ()
arrowSketch line ArrowSpec{..} = do
    when arrowDrawBody (lineSketch line)

    let Line start end = line

        arrowTip = start +. (arrowheadRelPos *. (end -. start))

    let arrowheadHalf (+-) = angledLine arrowTip (angleOfLine line +. rad pi +- arrowheadAngle) arrowheadSize
        Line _ arrowLeftEnd  = arrowheadHalf (+.)
        Line _ arrowRightEnd = arrowheadHalf (-.)
    case (arrowheadDrawRight, arrowheadDrawLeft) of
        (True, True) -> do
            moveToVec arrowLeftEnd
            lineToVec arrowTip
            lineToVec arrowRightEnd
        (False, True) -> do
            moveToVec arrowLeftEnd
            lineToVec arrowTip
        (True, False) -> do
            moveToVec arrowRightEnd
            lineToVec arrowTip
        (False, False) -> pure ()

-- | Convenience function to sketch a circle.
circleSketch
    :: Vec2     -- ^ Center
    -> Double -- ^ Radius
    -> Render ()
circleSketch (Vec2 x y) r = arc x y r 0 (2*pi)

-- | Sketch a cross like ×. Sometimes useful to decorate a line with for e.g.
-- strikethrough effects, or to contrast the o in tic tac toe.
--
-- When drawn with the same radius, it combines to ⨂ with a 'circleSketch'.
crossSketch
    :: Vec2     -- ^ Center
    -> Double -- ^ Radius
    -> Render ()
crossSketch center r = do
    let lowerRight = Geometry.transform (rotateAround center (deg 45)) (center +. Vec2 r 0)
        line1 = angledLine lowerRight (deg (45+180)) (2*r)
        line2 = Geometry.transform (rotateAround center (deg 90)) line1
    lineSketch line1
    lineSketch line2

-- | Sketch part of a circle.
arcSketch
    :: Vec2 -- ^ Center
    -> Double -- ^ Radius
    -> Angle -- ^ Starting angle (absolute)
    -> Angle -- ^ Ending angle (absolute)
    -> Render ()
arcSketch (Vec2 x y) r angleStart angleEnd
  = arc x y r (getRad angleStart) (getRad angleEnd)

-- | Sketch part of a circle.
arcSketchNegative
    :: Vec2 -- ^ Center
    -> Double -- ^ Radius
    -> Angle -- ^ Starting angle (absolute)
    -> Angle -- ^ Ending angle (absolute)
    -> Render ()
arcSketchNegative (Vec2 x y) r angleStart angleEnd
  = arcNegative x y r (getRad angleStart) (getRad angleEnd)

-- | Sketch the line defined by a sequence of points.
pathSketch :: Foldable f => f Vec2 -> Render ()
pathSketch = go . toList
  where
    go [] = pure ()
    go (Vec2 x y : vecs) = do
        moveTo x y
        for_ vecs (\(Vec2 x' y') -> lineTo x' y')

-- | Sketch a 'Polygon'.
polygonSketch :: Polygon -> Render ()
polygonSketch (Polygon []) = pure ()
polygonSketch (Polygon xs) = pathSketch xs >> closePath

-- | Sketch a 'BoundingBox', which is sometimes useful for debugging.
boundingBoxSketch :: BoundingBox -> Render ()
boundingBoxSketch (BoundingBox (Vec2 xlo ylo) (Vec2 xhi yhi)) = do
    let w = xhi - xlo
        h = yhi - ylo
    rectangle xlo ylo w h
    moveTo xlo ylo
    lineTo xhi yhi
    moveTo xhi ylo
    lineTo xlo yhi

-- | Draw a caresian coordinate system in range (x,x') (y,y'). Very useful for
-- prototyping.
cartesianCoordinateSystem :: Render ()
cartesianCoordinateSystem = cairoScope $ do
    let minMax :: (Int, Int)
        minMax = (-1000, 1000)
        (minX, maxX) = minMax
        (minY, maxY) = minMax
    let vec2 x y = Vec2 (fromIntegral x) (fromIntegral y)
    setLineWidth 1

    cairoScope $ do
        setColor (hsva 0 0 0 0.5)
        sequence_ [ lineSketch (Line (vec2 x minY) (vec2 x maxY))
                  | x <- [minX, minX+100 .. maxX] ]
        sequence_ [ lineSketch (Line (vec2 minX y) (vec2 maxX y))
                  | y <- [minY, minY+100 .. maxY] ]
        stroke

    cairoScope $ do
        setColor (hsva 0 0 0 0.2)
        setDash [4,6] 2
        sequence_ [ lineSketch (Line (vec2 x minY) (vec2 x maxY))
                  | x <- [minX, minX+10 .. maxX]
                  , mod x 100 /= 0 ]
        sequence_ [ lineSketch (Line (vec2 minX y) (vec2 maxX y))
                  | y <- [minY, minY+10 .. maxY]
                  , mod y 100 /= 0]
        stroke

    let centeredText :: Int -> Int -> String -> Render ()
        centeredText x y str = do
            moveTo (fromIntegral x) (fromIntegral y)
            showTextAligned HCenter VTop str
    setFontSize 8
    setColor (mathematica97 0)
    sequence_ [ centeredText x y (show x ++ "," ++ show y)
              | x <- [minX, minX+100 .. maxX]
              , y <- [minY, minY+100 .. maxY] ]

-- | Like 'cartesianCoordinateSystem', but with polar coordinates.
radialCoordinateSystem :: Vec2 -> Int -> Render ()
radialCoordinateSystem center maxR = cairoScope $ do
    let distance = fromIntegral
    setLineWidth 1
    setColor (hsv 0 0 0)
    sequence_ [ circleSketch center (distance r) >> stroke
              | r <- [100, 200 .. maxR] ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) (distance maxR)) >> stroke
              | angle <- init [0, 45 .. 360 :: Int] ]

    setColor (hsva 0 0 0 0.5)
    sequence_ [ circleSketch center (distance r) >> stroke
              | r <- [25, 50 .. maxR]
              , mod r 100 /= 0 ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) (distance maxR)) >> stroke
              | angle <- init [0, 15 .. 360 :: Int]
              , mod angle 45 /= 0 ]

-- | (width, height) of the current surface.
getSurfaceDimensions :: Num int => Render (int, int)
getSurfaceDimensions = withTargetSurface $ \s -> do
    w <- imageSurfaceGetWidth s
    h <- imageSurfaceGetHeight s
    pure (fromIntegral w,fromIntegral h)

-- | Temporarily draw using a different composition operator, such as
-- 'OperatorClear' to delete part of an image.
withOperator :: Operator -> Render a -> Render a
withOperator op render = do
    formerOp <- getOperator
    setOperator op
    result <- render
    setOperator formerOp
    pure result

-- | Render something with a new Cairo state, restoring the old one afterwards.
-- The state includes things like current color, line width, dashing, and
-- transformation matrix.
--
-- This function is often used to introduce a kind of scope for Cairo drawing
-- subsections, so the changes made there don’t leak into subseeuqnt parts of
-- the drawing.
--
-- For example, the following sets the line width to 2 temporarily; after the
-- inner block, it is reset to 1.
--
-- @
-- do
--     'setLineWidth' 1
--     'cairoScope' $ do
--         'setLineWidth' 2
--         'moveTo' 0 0 >> 'lineTo' 100 0  -- drawn with line width 1
--         'stroke'
--     'moveTo' 0 10 >> 'lineTo' 100 10    -- drawn with line width 1
--     'stroke'
-- @
cairoScope :: Render a -> Render a
cairoScope render = save *> render <* restore

-- | Render something as a group, as in encapsulate it in 'pushGroup' and
-- 'popGroupToSource'.
--
-- This is commonly used to avoid a less transparent area when overlapping two
-- transparent areas.
--
-- The naive way has the intersection of the two circles darker,
--
-- @
-- do
--     'setSourceRGBA' 0 0 0 0.5
--     'circleSketch' ('Vec2' 0 0) ('Distance' 10)
--     'fill'
--     'circleSketch' ('Vec2' 7 0) ('Distance' 10)
--     'fill'
-- @
--
-- On the other hand this will have the combination of the entire combined shape
-- drawn with 0.5 alpha:
--
-- @
-- 'grouped' ('paintWithAlpha' 0.5)
--     'setSourceRGBA' 0 0 0 1
--     'circleSketch' ('Vec2' 0 0) ('Distance' 10)
--     'fill'
--     'circleSketch' ('Vec2' 7 0) ('Distance' 10)
--     'fill'
-- @
grouped :: Render after -> Render a -> Render a
grouped afterwards render = pushGroup *> render <* popGroupToSource <* afterwards

data VAlign = VTop | VCenter | VBottom
data HAlign = HLeft | HCenter | HRight

-- | Like Cairo’s 'showText', but with alignment parameters.
showTextAligned
    :: CairoString string
    => HAlign -- ^ Horizontal alignment
    -> VAlign -- ^ Vertical alignment
    -> string -- ^ Text
    -> Render ()
showTextAligned hAlign vAlign str = do
    (w,h) <- do ex <- textExtents str
                pure (textExtentsWidth ex, textExtentsHeight ex)
    let dx = case hAlign of
            HLeft   -> 0
            HCenter -> -w/2
            HRight  -> -w
        dy = case vAlign of
            VTop    -> h
            VCenter -> h/2
            VBottom -> 0
    relMoveTo dx dy
    showText str
    newPath -- The text API is wonky, it kinda-sorta moves the pointer but not really.
            -- newPath clears the path, so we get no leaks from the text.
