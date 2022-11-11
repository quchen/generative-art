{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- | Cairo drawing backend.
module Draw (
    -- * SVG and PNG file handling
      render
    , CoordinateSystem(..)
    , coordinateSystem
    , haddockRender

    -- * Drawing presets
    , moveToVec
    , lineToVec
    , Sketch(..)
    , Arrow(..)
    , ArrowSpec(..)
    , Circle(..)
    , Cross(..)
    , PolyBezier(..)
    , arcSketch
    , arcSketchNegative

    -- * Colors
    , Colour, Color
    , AlphaColour, AlphaColor
    , CairoColor(..)
    , module Draw.Color

    -- ** Discrete color schemes
    -- $discreteColorSchemes
    , module Draw.Color.Schemes.Discrete

    -- ** Continuous color schemes
    -- $continuousColorSchemes
    , module Draw.Color.Schemes.Continuous

    -- * Temporary Cairo modifications
    , withOperator
    , cairoScope
    , grouped

    -- * Orientation helpers
    , cartesianCoordinateSystem
    , CartesianParams(..)
    , radialCoordinateSystem
    , PolarParams(..)

    -- * Transformations
    , fromCairoMatrix
    , toCairoMatrix

    -- * Text
    , module Draw.Text

    -- * Convenience
    , for_
    , module Data.Foldable
    , module Data.Default.Class
) where



import Control.Monad
import Data.Default.Class
import Data.Foldable
import Data.List
import Graphics.Rendering.Cairo        as C hiding (x, y)
import Graphics.Rendering.Cairo.Matrix (Matrix (..))

import Draw.Color
import Draw.Color.Schemes.Continuous
import Draw.Color.Schemes.Discrete
import Draw.NormalizeSvg
import Draw.Text
import Geometry                      as G



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

-- | Renders the drawing as PNG or SVG, depending on the file extension.
render
    :: FilePath
    -> Int -- ^ Height (px for PNG, pt for SVG)
    -> Int -- ^ Width (px for PNG, pt for SVG)
    -> Render ()
    -> IO ()
render filepath w h actions =
    withSurface (fromExtension filepath) filepath w h (\surface -> renderWith surface actions)

-- | Argument to 'coordinateSystem' to pick zero location and axis direction.
data CoordinateSystem
    = CairoStandard_ZeroTopLeft_XRight_YDown

        -- ^ __Left-handed coordinate system.__ Standard Cairo/computer graphics
        -- coordinates. Zero is on the top left, and the Y axis points downwards.
        -- Even though this is common in computer graphics, working geometrically
        -- with a left-handed coordinate system can be a bit awkward and
        -- surprising.
        --
        -- <<docs/haddock/Draw.hs/coordinate_system_cairo_standard.svg>>
        --
        -- === __(Expand to see the code for the picture)__
        -- >>> :{
        -- haddockRender "Draw.hs/coordinate_system_cairo_standard.svg" 100 80 $ do
        --     cairoScope $ do
        --         C.translate 10 10
        --         setColor black
        --         setLineWidth 1
        --         rectangle 0 0 80 60
        --         setDash [2,2] 0
        --         stroke
        --     coordinateSystem CairoStandard_ZeroTopLeft_XRight_YDown
        --     C.translate 10 10
        --     cairoScope $ do
        --         sketch (Arrow (angledLine zero (deg 0) 80) def)
        --         sketch (Arrow (angledLine zero (deg 90) 60) def)
        --         stroke
        --     cairoScope $ do
        --         setColor (mathematica97 1)
        --         let radius = 20
        --         arc 0 0 radius 0 (pi/2)
        --         sketch (Arrow (lineReverse (angledLine (Vec2 0 radius) (deg 0) 10))
        --                       def {_arrowDrawBody=False, _arrowheadSize=7})
        --         stroke
        -- :}
        -- docs/haddock/Draw.hs/coordinate_system_cairo_standard.svg

    | MathStandard_ZeroBottomLeft_XRight_YUp Double
        -- ^ __Right-handed coordinate system.__ Standard math coordinates, with
        -- zero on the bottom left. Needs the image’s height as arguments for
        -- technical reasons.
        --
        -- <<docs/haddock/Draw.hs/coordinate_system_math_standard.svg>>
        --
        -- === __(Expand to see the code for the picture)__
        -- >>> :{
        -- haddockRender "Draw.hs/coordinate_system_math_standard.svg" 100 80 $ do
        --     cairoScope $ do
        --         C.translate 10 10
        --         setColor black
        --         setLineWidth 1
        --         rectangle 0 0 80 60
        --         setDash [2,2] 0
        --         stroke
        --     coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp 80)
        --     C.translate 10 10
        --     cairoScope $ do
        --         sketch (Arrow (angledLine zero (deg 0) 80) def)
        --         sketch (Arrow (angledLine zero (deg 90) 60) def)
        --         stroke
        --     cairoScope $ do
        --         setColor (mathematica97 1)
        --         let radius = 20
        --         arc 0 0 radius 0 (pi/2)
        --         sketch (Arrow (lineReverse (angledLine (Vec2 0 radius) (deg 0) 10))
        --                       def {_arrowDrawBody=False, _arrowheadSize=7})
        --         stroke
        -- :}
        -- docs/haddock/Draw.hs/coordinate_system_math_standard.svg


    | MathStandard_ZeroCenter_XRight_YUp Double Double
        -- ^ __Right-handed coordinate system.__ Standard math coordinates, with
        -- zero in the center. Needs the image’s width and height as arguments for
        -- technical reasons.
        --
        -- <<docs/haddock/Draw.hs/coordinate_system_math_standard_centered.svg>>
        --
        -- === __(Expand to see the code for the picture)__
        -- >>> :{
        -- haddockRender "Draw.hs/coordinate_system_math_standard_centered.svg" 100 80 $ do
        --     cairoScope $ do
        --         C.translate 10 10
        --         setColor black
        --         setLineWidth 1
        --         rectangle 0 0 80 60
        --         setDash [2,2] 0
        --         stroke
        --     coordinateSystem (MathStandard_ZeroCenter_XRight_YUp 100 80)
        --     cairoScope $ do
        --         sketch (Arrow (angledLine zero (deg 0) 40) def)
        --         sketch (Arrow (angledLine zero (deg 90) 30) def)
        --         stroke
        --         setColor (mathematica97 0 `withOpacity` 0.3)
        --         sketch (angledLine zero (deg 0) (-40))
        --         sketch (angledLine zero (deg 90) (-30))
        --         stroke
        --     cairoScope $ do
        --         setColor (mathematica97 1)
        --         let radius = 15
        --         arc 0 0 radius 0 (pi/2)
        --         sketch (Arrow (lineReverse (angledLine (Vec2 0 radius) (deg 0) 10))
        --                       def {_arrowDrawBody=False, _arrowheadSize=7})
        --         stroke
        -- :}
        -- docs/haddock/Draw.hs/coordinate_system_math_standard_centered.svg

    deriving (Eq, Ord, Show)

instance Default CoordinateSystem where
    def = CairoStandard_ZeroTopLeft_XRight_YDown

-- | Choose a coordinate system.
coordinateSystem :: CoordinateSystem -> Render ()
coordinateSystem cosy = do
    C.identityMatrix
    case cosy of
        CairoStandard_ZeroTopLeft_XRight_YDown -> pure ()
        MathStandard_ZeroBottomLeft_XRight_YUp height -> do
            C.translate 0 height
            C.scale 1 (-1)
        MathStandard_ZeroCenter_XRight_YUp width height -> do
            C.translate 0 height
            C.scale 1 (-1)
            C.translate (width/2) (height/2)

-- | Render pictures for Haddock with doctests. Nomenclature: the 'FilePath' for
-- /Foo.Bar.Baz/ is /Foo\/Bar\/Baz.hs\/pic_name.svg/.
haddockRender :: FilePath -> Int -> Int -> Render () -> IO ()
haddockRender filename w h actions = do
    let filepath = "docs/haddock/" ++ filename
    render filepath w h $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp (fromIntegral h))
        cartesianCoordinateSystem def
            { _cartesianAlpha = 0.5
            , _renderAxisLabels=False
            , _renderHundreds=False
            }
        setColor (mathematica97 0)
        actions
    normalizeSvgFile filepath
    putStrLn filepath

-- | 'Vec2'-friendly version of Cairo’s 'moveTo'.
moveToVec :: Vec2 -> Render ()
moveToVec (Vec2 x y) = moveTo x y

-- | 'Vec2'-friendly version of Cairo’s 'lineTo'.
lineToVec :: Vec2 -> Render ()
lineToVec (Vec2 x y) = lineTo x y

lineSketch :: Line -> Render ()
lineSketch = sketch
{-# DEPRECATED lineSketch "use `sketch` instead" #-}

-- |
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_Bezier.svg" 150 100 $ do
--     sketch (Bezier (Vec2 10 10) (Vec2 50 200) (Vec2 100 (-50)) (Vec2 140 90))
--     stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_Bezier.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_Bezier.svg>>
instance Sketch Bezier where
    sketch (Bezier start (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3)) = do
        moveToVec start
        curveTo x1 y1 x2 y2 x3 y3

newtype PolyBezier f = PolyBezier (f Bezier)

-- | Sketch a continuous curve consisting of multiple Bezier segments. The end of
-- each segment is assumed to be the start of the next one.
instance Sequential f => Sketch (PolyBezier f) where
    sketch (PolyBezier xs) = go (toList xs)
      where
        go [] = pure ()
        go ps@(Bezier start _ _ _ : _) = do
            moveToVec start
            for_ ps $ \(Bezier _ (Vec2 x1 y1) (Vec2 x2 y2) (Vec2 x3 y3)) -> curveTo x1 y1 x2 y2 x3 y3

data ArrowSpec = ArrowSpec
    { _arrowheadRelPos    :: !Double -- ^ Relative position of the arrow head, from 0 (start) to 1 (end). 0.5 paints the arrow in the center. ('def'ault: 1)
    , _arrowheadSize      :: !Double -- ^ Length of each of the sides of the arrow head. ('def'ault: 10)
    , _arrowDrawBody      :: !Bool   -- ^ Draw the arrow’s main body line ('True'), or just the tip ('False')? ('def'ault: 'True')
    , _arrowheadAngle     :: !Angle  -- ^ How pointy should the arrow be? 10° is very pointy, 80° very blunt. ('def'ault: @'rad' 0.5@)
    , _arrowheadDrawRight :: !Bool   -- ^ Draw the left part of the arrow head? ('def'ault: 'True')
    , _arrowheadDrawLeft  :: !Bool   -- ^ Draw the right part of the arrow head? ('def'ault: 'True')
    } deriving (Eq, Show)

instance Default ArrowSpec where
    def = ArrowSpec
        { _arrowheadRelPos    = 1
        , _arrowheadSize      = 10
        , _arrowDrawBody      = True
        , _arrowheadAngle     = rad 0.5
        , _arrowheadDrawRight = True
        , _arrowheadDrawLeft  = True
        }

-- | For 'sketch'ing arrows.
data Arrow = Arrow !Line !ArrowSpec
    deriving (Eq, Show)

-- |
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_Arrow.svg" 150 100 $ do
--     sketch (Arrow (Line (Vec2 10 10) (Vec2 140 90)) def)
--     stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_Arrow.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_Arrow.svg>>
instance Sketch Arrow where
    sketch (Arrow line ArrowSpec{..}) = do
        when _arrowDrawBody (sketch line)

        let Line start end = line

            arrowTip = start +. (_arrowheadRelPos *. (end -. start))

        let arrowheadHalf (+-) = angledLine arrowTip (angleOfLine line +. rad pi +- _arrowheadAngle) _arrowheadSize
            Line _ arrowLeftEnd  = arrowheadHalf (+.)
            Line _ arrowRightEnd = arrowheadHalf (-.)
        case (_arrowheadDrawRight, _arrowheadDrawLeft) of
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

-- | Sketch a shape that can then be made visible by drawing functions such as 'stroke' or 'fill'.
class Sketch a where
    sketch :: a -> Render ()

instance (Sketch a, Sketch b) => Sketch (Either a b) where
    sketch (Left l) = sketch l
    sketch (Right r) = sketch r

instance (Sketch a, Sketch b) => Sketch (a,b) where
    sketch (a,b) = sketch a >> sketch b

instance (Sketch a, Sketch b, Sketch c) => Sketch (a,b,c) where
    sketch (a,b,c) = sketch a >> sketch b >> sketch c

instance (Sketch a, Sketch b, Sketch c, Sketch d) => Sketch (a,b,c,d) where
    sketch (a,b,c,d) = sketch a >> sketch b >> sketch c >> sketch d

instance (Sketch a, Sketch b, Sketch c, Sketch d, Sketch e) => Sketch (a,b,c,d,e) where
    sketch (a,b,c,d,e) = sketch a >> sketch b >> sketch c >> sketch d >> sketch e

instance Sketch a => Sketch [a] where
    sketch xs = for_ xs sketch

-- |
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_Line.svg" 150 100 $ do
--     sketch (Line (Vec2 10 10) (Vec2 140 90))
--     stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_Line.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_Line.svg>>
instance Sketch Line where
    sketch (Line start end) = do
        moveToVec start
        lineToVec end

-- | Polyline, i.e. a sequence of lines given by their joints.
--
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_Sequential_Vec2.svg" 150 100 $ do
--     sketch (Polyline [Vec2 10 10, Vec2 90 90, Vec2 120 10, Vec2 140 50])
--     stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_Sequential_Vec2.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_Sequential_Vec2.svg>>
instance Sequential f => Sketch (Polyline f) where
    sketch (Polyline xs) = go (toList xs)
      where
        go [] = pure ()
        go (Vec2 x0 y0 : vecs) = do
            moveTo x0 y0
            for_ vecs (\(Vec2 x y) -> lineTo x y)

-- |
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_Polygon.svg" 100 100 $ do
--     sketch (Polygon [Vec2 20 10, Vec2 10 80, Vec2 45 45, Vec2 60 90, Vec2 90 30])
--     stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_Polygon.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_Polygon.svg>>
instance Sketch Polygon where
    sketch (Polygon []) = pure ()
    sketch (Polygon xs) = sketch (Polyline xs) >> closePath

-- |
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_Circle.svg" 100 100 $ do
--     sketch (Circle (Vec2 50 50) 45)
--     stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_Circle.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_Circle.svg>>
instance Sketch Circle where
    sketch (Circle (Vec2 x y) r) = arc x y r 0 (2*pi)

-- |
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_Ellipse.svg" 150 100 $ do
--     sketch (G.transform (G.translate (Vec2 75 50) <> G.rotate (deg 20) <> G.scale' 1.4 0.9)
--                         (toEllipse (Circle zero 45)))
--     stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_Ellipse.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_Ellipse.svg>>
instance Sketch Ellipse where
    sketch (Ellipse t) = cairoScope $ do
        C.transform (toCairoMatrix t)
        sketch (Circle zero 1)

-- | 'sketch' a cross like ×. Sometimes useful to decorate a line with for e.g.
-- strikethrough effects, or to contrast the o in tic tac toe.
--
-- When drawn with the same radius, it combines to ⨂ with a 'Circle'.
data Cross = Cross
    { _crossCenter :: !Vec2
    , _crossRadius :: !Double
    } deriving (Eq, Ord, Show)

-- |
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_Cross.svg" 90 40 $ do
--     sketch (Cross  (Vec2 20 20) 15) >> stroke
--     sketch (Cross  (Vec2 60 20) 15) >> stroke
--     sketch (Circle (Vec2 60 20) 15) >> stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_Cross.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_Cross.svg>>
instance Sketch Cross where
    sketch (Cross center r) = do
        let lowerRight = G.transform (rotateAround center (deg 45)) (center +. Vec2 r 0)
            line1 = angledLine lowerRight (deg (45+180)) (2*r)
            line2 = G.transform (rotateAround center (deg 90)) line1
        sketch line1
        sketch line2

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

-- | Sketches a rectangle with a diagonal cross through it. Useful for debugging.
--
-- >>> :{
-- haddockRender "Draw.hs/instance_Sketch_BoundingBox.svg" 100 100 $ do
--     let geometry = [Circle (Vec2 30 30) 25, Circle (Vec2 60 60) 35]
--     for_ geometry $ \x -> cairoScope (sketch x >> setColor (mathematica97 1) >> setDash [4,6] 0 >> stroke)
--     sketch (boundingBox geometry)
--     stroke
-- :}
-- docs/haddock/Draw.hs/instance_Sketch_BoundingBox.svg
--
-- <<docs/haddock/Draw.hs/instance_Sketch_BoundingBox.svg>>
instance Sketch BoundingBox where
    sketch (BoundingBox (Vec2 xlo ylo) (Vec2 xhi yhi)) = do
        let w = xhi - xlo
            h = yhi - ylo
        rectangle xlo ylo w h
        moveTo xlo ylo
        lineTo xhi yhi
        moveTo xhi ylo
        lineTo xlo yhi

data CartesianParams = CartesianParams
    { _cartesianMinX    :: !Int
    , _cartesianMaxX    :: !Int
    , _cartesianMinY    :: !Int
    , _cartesianMaxY    :: !Int
    , _cartesianAlpha   :: !Double
    , _renderAxisLabels :: !Bool -- ^ Render numbers to the hundreds intersections?
    , _renderTens       :: !Bool -- ^ Render the tens (as crosses)?
    , _renderHundreds   :: !Bool -- ^ Render the hundreds lines more visible?
    } deriving (Eq, Ord, Show)

instance Default CartesianParams where
    def = CartesianParams
        { _cartesianMinX    = -1000
        , _cartesianMaxX    =  1000
        , _cartesianMinY    = -1000
        , _cartesianMaxY    =  1000
        , _cartesianAlpha   =  1
        , _renderAxisLabels =  True
        , _renderTens       =  True
        , _renderHundreds   =  True
        }

-- | Draw a caresian coordinate system in range (x,x') (y,y'). Very useful for
-- prototyping.
--
-- >>> :{
-- haddockRender "Draw.hs/cartesianCoordinateSystem.svg" 320 220 (cartesianCoordinateSystem def)
-- :}
-- docs/haddock/Draw.hs/cartesianCoordinateSystem.svg
--
-- <<docs/haddock/Draw.hs/cartesianCoordinateSystem.svg>>
cartesianCoordinateSystem :: CartesianParams -> Render ()
cartesianCoordinateSystem params@CartesianParams{..}  = grouped (paintWithAlpha _cartesianAlpha) $ do
    let vec2 x y = Vec2 (fromIntegral x) (fromIntegral y)
    setLineWidth 1

    let CartesianParams{_cartesianMinX=minX, _cartesianMaxX=maxX, _cartesianMinY=minY, _cartesianMaxY=maxY} = params

    when _renderHundreds $ cairoScope $ do
        setColor (hsva 0 0 0 0.5)
        sequence_ [ sketch (Line (vec2 x minY) (vec2 x maxY))
                | x <- [minX, minX+100 .. maxX] ]
        sequence_ [ sketch (Line (vec2 minX y) (vec2 maxX y))
                | y <- [minY, minY+100 .. maxY] ]
        stroke

    when _renderTens $ cairoScope $ do
        setColor (hsva 0 0 0 0.2)
        setDash [4,6] 2
        let skipHundreds i = not _renderHundreds || mod i 100 /= 0
        sequence_ [ sketch (Line (vec2 x minY) (vec2 x maxY))
                  | x <- [minX, minX+10 .. maxX]
                  , skipHundreds x ]
        sequence_ [ sketch (Line (vec2 minX y) (vec2 maxX y))
                  | y <- [minY, minY+10 .. maxY]
                  , skipHundreds y]
        stroke

    when _renderAxisLabels $ cairoScope $ do
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
    { _polarCenter    :: !Vec2
    , _polarMaxRadius :: !Double
    , _polarAlpha     :: !Double
    } deriving (Eq, Ord, Show)

instance Default PolarParams where
    def = PolarParams
        { _polarCenter    = zero
        , _polarMaxRadius = 1000
        , _polarAlpha     = 1
        }

-- | Like 'cartesianCoordinateSystem', but with polar coordinates.
--
-- >>> :{
-- haddockRender "Draw.hs/radialCoordinateSystem.svg" 250 250 $ do
--     C.translate 50 50
--     radialCoordinateSystem def
-- :}
-- docs/haddock/Draw.hs/radialCoordinateSystem.svg
--
-- <<docs/haddock/Draw.hs/radialCoordinateSystem.svg>>
radialCoordinateSystem :: PolarParams -> Render ()
radialCoordinateSystem PolarParams{_polarCenter=center, _polarMaxRadius=maxR} = cairoScope $ do
    setLineWidth 1
    setColor (hsv 0 0 0)
    sequence_ [ sketch (Circle center (fromIntegral r)) >> stroke
              | r <- [100, 200 .. ceiling maxR :: Int] ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) maxR) >> stroke
              | angle <- init [0, 45 .. 360 :: Int] ]

    setColor (hsva 0 0 0 0.5)
    sequence_ [ sketch (Circle center (fromIntegral r)) >> stroke
              | r <- [25, 50 .. ceiling maxR :: Int]
              , mod r 100 /= 0 ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) maxR) >> stroke
              | angle <- init [0, 15 .. 360 :: Int]
              , mod angle 45 /= 0 ]

-- | Temporarily draw using a different composition operator, such as
-- 'OperatorClear' to delete part of an image.
withOperator :: Operator -> Render a -> Render a
withOperator op actions = do
    formerOp <- getOperator
    setOperator op
    result <- actions
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
cairoScope actions = save *> actions <* restore

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
grouped afterwards actions = cairoScope $ pushGroup *> actions <* popGroupToSource <* afterwards

-- | Translate between Cairo and our matrix representation for transformations.
--
-- Cairo does its transformation in inverse: we transform the geometry, Cairo
-- transforms the canvas. 'fromCairoMatrix' and 'toCairoMatrix' translate between
-- the worlds, so that conceptually both of these yield the same output:
--
-- @
-- trafo :: 'Transformation'
--
-- 'sketch' ('transform' trafo geometry)
-- --
-- 'C.transform' ('fromCairoMatrix' trafo) '>>' 'sketch' geometry
-- @
--
-- __Note__ that Cairo’s 'C.transform' does more than just moving around lines: it
-- also scales other properties such as line width, so the pictures described above
-- might have some differences.
--
-- Useful Cairo functions for working with this are
--
-- @
-- 'C.transform' :: 'C.Matrix' -> 'C.Render' ()
-- 'C.setMatrix' :: 'C.Matrix' -> 'C.Render' ()
-- 'C.getMatrix' :: 'C.Render' 'C.Matrix'
-- @
fromCairoMatrix :: Matrix -> Transformation
fromCairoMatrix (Matrix ca cb cc cd ce cf) =
    -- According to the Haskell Cairo docs:
    --
    -- > Matrix a b c d e f:
    -- >   / x' \  =  / a c \  / x \  + / e \
    -- >   \ y' /     \ b d /  \ y /    \ f /
    --
    -- Our matrix representation is (copied from the 'Transformation' doc
    -- block in "Geometry.Core"):
    --
    -- > transformation a b c
    -- >                d e f
    -- >
    -- >   / x' \  =  / a b \  / x \  + / c \
    -- >   \ y' /     \ d e /  \ y /    \ f /
    let a = ca
        b = cc
        c = ce
        d = cb
        e = cd
        f = cf
    in Transformation (Mat2 a b d e) (Vec2 c f)

-- | See  'fromCairoMatrix'’ documentation, of which 'toCairoMatrix' is the inverse.
toCairoMatrix :: Transformation -> C.Matrix
toCairoMatrix trafo =
    let Transformation (Mat2 a b d e) (Vec2 c f) = trafo
        ca = a
        cc = b
        ce = c
        cb = d
        cd = e
        cf = f
    in Matrix ca cb cc cd ce cf

-- $discreteColorSchemes
--
-- Discrete color schemes, taken from:
--
--  * Mathematica: https://www.wolfram.com/mathematica/
--  * Color Brewer 2: https://colorbrewer2.org/
--
-- +-----------------+--------------------------------------------------------------+---------+
-- | Name            |                                                              | Domain  |
-- +=================+==============================================================+=========+
-- | 'mathematica97' | <<docs/colors/schemes/discrete/mathematica/ColorData97.svg>> | [0..∞)  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'accent'        | <<docs/colors/schemes/discrete/colorbrewer2/accent.svg>>     | [0..7]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'dark2'         | <<docs/colors/schemes/discrete/colorbrewer2/dark2.svg>>      | [0..7]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'paired'        | <<docs/colors/schemes/discrete/colorbrewer2/paired.svg>>     | [0..11] |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'pastel1'       | <<docs/colors/schemes/discrete/colorbrewer2/pastel1.svg>>    | [0..8]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'pastel2'       | <<docs/colors/schemes/discrete/colorbrewer2/pastel2.svg>>    | [0..7]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'set1'          | <<docs/colors/schemes/discrete/colorbrewer2/set1.svg>>       | [0..8]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'set2'          | <<docs/colors/schemes/discrete/colorbrewer2/set2.svg>>       | [0..7]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'set3'          | <<docs/colors/schemes/discrete/colorbrewer2/set3.svg>>       | [0..11] |
-- +-----------------+--------------------------------------------------------------+---------+

-- $continuousColorSchemes
--
-- Continuous color schemes, taken from:
--
--  * Color Brewer 2: https://colorbrewer2.org/
--  * Matplotlib: https://matplotlib.org/
--  * Seaborn: https://seaborn.pydata.org/
--
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | Name              |                                                                   | Domain | Type     |
-- +===================+===================================================================+========+==========+
-- | 'haskell'         | <<docs/colors/schemes/continuous/haskell/logo.png>>               | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'magma'           | <<docs/colors/schemes/continuous/matplotlib/magma.png>>           | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'inferno'         | <<docs/colors/schemes/continuous/matplotlib/inferno.png>>         | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'plasma'          | <<docs/colors/schemes/continuous/matplotlib/plasma.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'viridis'         | <<docs/colors/schemes/continuous/matplotlib/viridis.png>>         | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'cividis'         | <<docs/colors/schemes/continuous/matplotlib/cividis.png>>         | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'turbo'           | <<docs/colors/schemes/continuous/matplotlib/turbo.png>>           | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'twilight'        | <<docs/colors/schemes/continuous/matplotlib/twilight.png>>        | [0..1] | Cyclic   |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'rocket'          | <<docs/colors/schemes/continuous/seaborn/rocket.png>>             | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'mako'            | <<docs/colors/schemes/continuous/seaborn/mako.png>>               | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'flare'           | <<docs/colors/schemes/continuous/seaborn/flare.png>>              | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'crest'           | <<docs/colors/schemes/continuous/seaborn/crest.png>>              | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'vlag'            | <<docs/colors/schemes/continuous/seaborn/vlag.png>>               | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'icefire'         | <<docs/colors/schemes/continuous/seaborn/icefire.png>>            | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'orRd'            | <<docs/colors/schemes/continuous/colorbrewer2/orRd.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'puBu'            | <<docs/colors/schemes/continuous/colorbrewer2/puBu.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'buPu'            | <<docs/colors/schemes/continuous/colorbrewer2/buPu.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'oranges'         | <<docs/colors/schemes/continuous/colorbrewer2/oranges.png>>       | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'buGn'            | <<docs/colors/schemes/continuous/colorbrewer2/buGn.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'ylOrBr'          | <<docs/colors/schemes/continuous/colorbrewer2/ylOrBr.png>>        | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'ylGn'            | <<docs/colors/schemes/continuous/colorbrewer2/ylGn.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'reds'            | <<docs/colors/schemes/continuous/colorbrewer2/reds.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'rdPu'            | <<docs/colors/schemes/continuous/colorbrewer2/rdPu.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'greens'          | <<docs/colors/schemes/continuous/colorbrewer2/greens.png>>        | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'ylGnBu'          | <<docs/colors/schemes/continuous/colorbrewer2/ylGnBu.png>>        | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'purples'         | <<docs/colors/schemes/continuous/colorbrewer2/purples.png>>       | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'gnBu'            | <<docs/colors/schemes/continuous/colorbrewer2/gnBu.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'greys'           | <<docs/colors/schemes/continuous/colorbrewer2/greys.png>>         | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'ylOrRd'          | <<docs/colors/schemes/continuous/colorbrewer2/ylOrRd.png>>        | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'puRd'            | <<docs/colors/schemes/continuous/colorbrewer2/puRd.png>>          | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'blues'           | <<docs/colors/schemes/continuous/colorbrewer2/blues.png>>         | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'puBuGn'          | <<docs/colors/schemes/continuous/colorbrewer2/puBuGn.png>>        | [0..1] | Monotone |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'spectral'        | <<docs/colors/schemes/continuous/colorbrewer2/spectral.png>>      | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'rdYlGn'          | <<docs/colors/schemes/continuous/colorbrewer2/rdYlGn.png>>        | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'rdBu'            | <<docs/colors/schemes/continuous/colorbrewer2/rdBu.png>>          | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'piYG'            | <<docs/colors/schemes/continuous/colorbrewer2/piYG.png>>          | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'pRGn'            | <<docs/colors/schemes/continuous/colorbrewer2/pRGn.png>>          | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'rdYlBu'          | <<docs/colors/schemes/continuous/colorbrewer2/rdYlBu.png>>        | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'brBG'            | <<docs/colors/schemes/continuous/colorbrewer2/brBG.png>>          | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'rdGy'            | <<docs/colors/schemes/continuous/colorbrewer2/rdGy.png>>          | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
-- | 'puOr'            | <<docs/colors/schemes/continuous/colorbrewer2/puOr.png>>          | [0..1] | Divisive |
-- +-------------------+-------------------------------------------------------------------+--------+----------+
