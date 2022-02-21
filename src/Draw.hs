{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Cairo drawing backend.
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
    , Sketch(..)
    , lineSketch
    , bezierSketch
    , Arrow(..)
    , ArrowSpec(..)
    , arrowSketch
    , circleSketch
    , Cross(..)
    , crossSketch
    , arcSketch
    , arcSketchNegative
    , polygonSketch
    , pathSketch
    , boundingBoxSketch

    -- * Orientation helpers
    , cartesianCoordinateSystem
    , CartesianParams(..)
    , radialCoordinateSystem
    , PolarParams(..)

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



import Control.Monad
import Data.Default.Class
import Data.Foldable
import Data.List
import Graphics.Rendering.Cairo as C hiding (x, y)

import Draw.Color
import Draw.Color.Schemes.Continuous
import Draw.Color.Schemes.Discrete
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
    -> (Surface -> IO a) -- ^ Drawing action, see 'C.renderWith'
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

lineSketch :: Line -> Render ()
lineSketch = sketch
{-# DEPRECATED lineSketch "use `sketch` instead" #-}

-- | Sketch a continuous curve consisting of multiple Bezier segments. The end of
-- each segment is assumed to be the start of the next one.
instance Sequential f => Sketch (f Bezier) where
    sketch = go . toList
      where
        go [] = pure ()
        go (ps@(Bezier start _ _ _ : _)) = do
            moveToVec start
            for_ ps $ \(Bezier _ (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3)) -> curveTo x1 y1 x2 y2 x3 y3

bezierSketch :: Sequential f => f Bezier -> Render ()
bezierSketch = sketch
{-# DEPRECATED bezierSketch "use `sketch` instead" #-}

data ArrowSpec = ArrowSpec
    { arrowheadRelPos    :: !Double -- ^ Relative position of the arrow head, from 0 (start) to 1 (end). 0.5 paints the arrow in the center. ('def'ault: 1)
    , arrowheadSize      :: !Double -- ^ Length of each of the sides of the arrow head. ('def'ault: 10)
    , arrowDrawBody      :: !Bool   -- ^ Draw the arrow’s main body line ('True'), or just the tip ('False')? ('def'ault: 'True')
    , arrowheadAngle     :: !Angle  -- ^ How pointy should the arrow be? 10° is very pointy, 80° very blunt. ('def'ault: @'rad' 0.5@)
    , arrowheadDrawRight :: !Bool   -- ^ Draw the left part of the arrow head? ('def'ault: 'True')
    , arrowheadDrawLeft  :: !Bool   -- ^ Draw the right part of the arrow head? ('def'ault: 'True')
    } deriving (Eq, Show)

instance Default ArrowSpec where
    def = ArrowSpec
        { arrowheadRelPos    = 1
        , arrowheadSize      = 10
        , arrowDrawBody      = True
        , arrowheadAngle     = rad 0.5
        , arrowheadDrawRight = True
        , arrowheadDrawLeft  = True
        }

-- | For 'sketch'ing arrows.
data Arrow = Arrow !Line !ArrowSpec
    deriving (Eq, Show)

instance Sketch Arrow where
    sketch (Arrow line ArrowSpec{..}) = do
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

arrowSketch :: Line -> ArrowSpec -> Render ()
arrowSketch line spec = sketch (Arrow line spec)
{-# DEPRECATED arrowSketch "use `sketch (Arrow line spec)` instead" #-}

-- | Sketch a shape that can then be made visible by drawing functions such as 'stroke' or 'fill'.
class Sketch a where
    sketch :: a -> Render ()

instance Sketch Line where
    sketch (Line start end) = do
        moveToVec start
        lineToVec end

-- | Trajectory given by its points
instance Sequential f => Sketch (f Vec2) where
    sketch = go . toList
      where
        go [] = pure ()
        go (Vec2 x0 y0 : vecs) = do
            moveTo x0 y0
            for_ vecs (\(Vec2 x y) -> lineTo x y)

instance Sketch Polygon where
    sketch (Polygon []) = pure ()
    sketch (Polygon xs) = sketch xs >> closePath

-- | For 'sketch'ing circles.
data Circle = Circle
    { _circleCenter :: !Vec2
    , _circleRadius :: !Double
    } deriving (Eq, Ord, Show)

instance Sketch Circle where
    sketch (Circle (Vec2 x y) r) = arc x y r 0 (2*pi)

circleSketch :: Vec2 -> Double -> Render ()
circleSketch center r = sketch (Circle center r)
{-# DEPRECATED circleSketch "use `sketch (Circle center radius)` instead" #-}

-- | 'sketch' a cross like ×. Sometimes useful to decorate a line with for e.g.
-- strikethrough effects, or to contrast the o in tic tac toe.
--
-- When drawn with the same radius, it combines to ⨂ with a 'Circle'.
data Cross = Cross
    { _crossCenter :: !Vec2
    , _crossRadius :: !Double
    } deriving (Eq, Ord, Show)

instance Sketch Cross where
    sketch (Cross center r) = do
        let lowerRight = Geometry.transform (rotateAround center (deg 45)) (center +. Vec2 r 0)
            line1 = angledLine lowerRight (deg (45+180)) (2*r)
            line2 = Geometry.transform (rotateAround center (deg 90)) line1
        sketch line1
        sketch line2

crossSketch
    :: Vec2
    -> Double
    -> Render ()
crossSketch center r = sketch (Cross center r)
{-# DEPRECATED crossSketch "use `sketch (Cross center r)` instead" #-}

-- | Sketch part of a circle.
arcSketch
    :: Vec2   -- ^ Center
    -> Double -- ^ Radius
    -> Angle  -- ^ Starting angle (absolute)
    -> Angle  -- ^ Ending angle (absolute)
    -> Render ()
arcSketch (Vec2 x y) r angleStart angleEnd
  = arc x y r (getRad angleStart) (getRad angleEnd)

-- | Sketch part of a circle.
arcSketchNegative
    :: Vec2   -- ^ Center
    -> Double -- ^ Radius
    -> Angle  -- ^ Starting angle (absolute)
    -> Angle  -- ^ Ending angle (absolute)
    -> Render ()
arcSketchNegative (Vec2 x y) r angleStart angleEnd
  = arcNegative x y r (getRad angleStart) (getRad angleEnd)

pathSketch :: Sequential f => f Vec2 -> Render ()
pathSketch = sketch
{-# DEPRECATED pathSketch "use `sketch` instead" #-}

polygonSketch :: Polygon -> Render ()
polygonSketch = sketch
{-# DEPRECATED polygonSketch "use `sketch` instead" #-}

-- | Sketches a ectangle with a diagonal cross through it. Useful for debugging.
instance Sketch BoundingBox where
    sketch (BoundingBox (Vec2 xlo ylo) (Vec2 xhi yhi)) = do
        let w = xhi - xlo
            h = yhi - ylo
        rectangle xlo ylo w h
        moveTo xlo ylo
        lineTo xhi yhi
        moveTo xhi ylo
        lineTo xlo yhi

boundingBoxSketch :: BoundingBox -> Render ()
boundingBoxSketch = sketch
{-# DEPRECATED boundingBoxSketch "use `sketch` instead" #-}

data CartesianParams = CartesianParams
    { _cartesianMinX :: !Int
    , _cartesianMaxX :: !Int
    , _cartesianMinY :: !Int
    , _cartesianMaxY :: !Int
    } deriving (Eq, Ord, Show)

instance Default CartesianParams where
    def = CartesianParams
        { _cartesianMinX = -1000
        , _cartesianMaxX =  1000
        , _cartesianMinY = -1000
        , _cartesianMaxY =  1000
        }

-- | Draw a caresian coordinate system in range (x,x') (y,y'). Very useful for
-- prototyping.
--
-- @
-- 'cartesianCoordinateSystem' 'def'
-- @
cartesianCoordinateSystem :: CartesianParams -> Render ()
cartesianCoordinateSystem CartesianParams{_cartesianMinX=minX, _cartesianMaxX=maxX, _cartesianMinY=minY, _cartesianMaxY=maxY} = cairoScope $ do
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

data PolarParams = PolarParams
    { _polarCenter :: !Vec2
    , _polarMaxRadius :: !Double
    } deriving (Eq, Ord, Show)

instance Default PolarParams where
    def = PolarParams zero 1000

-- | Like 'cartesianCoordinateSystem', but with polar coordinates.
--
-- @
-- 'radialCoordinateSystem' 'def'
-- @
radialCoordinateSystem :: PolarParams -> Render ()
radialCoordinateSystem PolarParams{_polarCenter=center, _polarMaxRadius=maxR} = cairoScope $ do
    setLineWidth 1
    setColor (hsv 0 0 0)
    sequence_ [ circleSketch center (fromIntegral r) >> stroke
              | r <- [100, 200 .. ceiling maxR :: Int] ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) maxR) >> stroke
              | angle <- init [0, 45 .. 360 :: Int] ]

    setColor (hsva 0 0 0 0.5)
    sequence_ [ circleSketch center (fromIntegral r) >> stroke
              | r <- [25, 50 .. ceiling maxR :: Int]
              , mod r 100 /= 0 ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) maxR) >> stroke
              | angle <- init [0, 15 .. 360 :: Int]
              , mod angle 45 /= 0 ]

-- | Temporarily draw using a different composition operator, such as
-- 'OperatorClear' to delete part of an image.
withOperator :: Operator -> Render a -> Render a
withOperator op render = do
    formerOp <- getOperator
    setOperator op
    result <- render
    setOperator formerOp
    pure result

-- | Open a new Cairo scope to allow local parameter changes. When the scope
-- closes, the parameters are reset. Cairo documentation hides what actually is in
-- the parameter state remarkably well; the state thus includes
-- [(source)](https://github.com\/freedesktop\/cairo\/blob\/2ec0a874031fdb2f3d7a4eaf1d63740a0e25b268\/src\/cairo-gstate-private.h#L41):
--
--   * Drawing operator ('withOperator')
--   * Tolerance ('setTolerance')
--   * Antialiasing ('setAntialias')
--   * Line style ('setLineWidth', 'setLineCap', 'setLineJoin', 'setMiterLimit', 'setDash')
--   * Fill rule ('setFillRule')
--   * Font face, scaling, options
--   * Clipping ('clip')
--   * Pattern (includes colors\/'setColor', gradients\/'withLinearPattern' etc.)
--   * Tranformation matrix ('C.translate' etc.)
--
-- For example, the following sets the line width to 2 temporarily; after the
-- inner block, it is reset to 1.
--
-- @
-- 'setLineWidth' 1
--
-- 'cairoScope' $ do
--     'setLineWidth' 2
--     'sketch' ('Line' ('Vec2' 0 0) ('Vec2' 100 0)) -- drawn with line width 2
--     'stroke'
--
-- 'sketch' ('Line' ('Vec2' 0 10) ('Vec2' 100 10))   -- drawn with line width 1
-- 'stroke'
-- @
cairoScope :: Render a -> Render a
cairoScope render = save *> render <* restore

-- | Render something as a group, as in encapsulate it in 'pushGroup' and
-- 'popGroupToSource'. This function semantically includes a call 'cairoScope'.
--
-- 'grouped' is commonly used to avoid a less transparent area when overlapping two
-- transparent areas.
--
-- The naive way has the intersection of the two circles darker,
--
-- @
-- do
--     'setSourceRGBA' 0 0 0 0.5
--     'sketch' ('Circle' ('Vec2' 0 0) 10)
--     'fill'
--     'sketch' ('Circle' ('Vec2' 7 0) 10)
--     'fill'
-- @
--
-- On the other hand this will have the combination of the entire combined shape
-- drawn with 0.5 alpha:
--
-- @
-- 'grouped' ('paintWithAlpha' 0.5) $ do
--     'setSourceRGBA' 0 0 0 1
--     'sketch' ('Circle' ('Vec2' 0 0) 10)
--     'fill'
--     'sketch' ('Circle' ('Vec2' 7 0) 10)
--     'fill'
-- @
grouped :: Render after -> Render a -> Render a
grouped afterwards render = pushGroup *> render <* popGroupToSource <* afterwards

-- | Vertical alignment
data VAlign = VTop | VCenter | VBottom deriving (Eq, Ord, Show)

-- | Horizontal alignment
data HAlign = HLeft | HCenter | HRight deriving (Eq, Ord, Show)

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
