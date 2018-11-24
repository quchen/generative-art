{-# LANGUAGE RecordWildCards #-}

module Draw (
    -- * Colors
      hsva
    , mmaColor

    -- * Drawing presets
    , lineSketch
    , ArrowSpec(..)
    , arrowSketch
    , circleSketch
    , crossSketch
    , arcSketch
    , polygonSketch

    -- * Orientation helpers
    , cartesianCoordinateSystem
    , radialCoordinateSystem

    -- * Temporary Cairo modifications
    , withOperator
    , withSavedState
    , grouped

    -- * Text
    , showTextAligned
    , HAlign(..)
    , VAlign(..)

    -- * Convenience
    , module Data.Default.Class
)where



import Control.Monad
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Default.Class
import Data.Foldable
import Graphics.Rendering.Cairo hiding (x, y)

import Geometry



hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v a = setSourceRGBA channelRed channelGreen channelBlue a
    where RGB{..} = hsv h s v

-- | Mathematica’s ColorData[97] scheme.
mmaColor :: Int -> Double -> Render ()
mmaColor n alpha = setSourceRGBA r g b alpha
  where
    (r,g,b) = case rem n 15 of
        0 ->  (0.368417, 0.506779, 0.709798)
        1 ->  (0.880722, 0.611041, 0.142051)
        2 ->  (0.560181, 0.691569, 0.194885)
        3 ->  (0.922526, 0.385626, 0.209179)
        4 ->  (0.528488, 0.470624, 0.701351)
        5 ->  (0.772079, 0.431554, 0.102387)
        6 ->  (0.363898, 0.618501, 0.782349)
        7 ->  (1, 0.75, 0)
        8 ->  (0.647624, 0.37816, 0.614037)
        9 ->  (0.571589, 0.586483, 0)
        10 -> (0.915, 0.3325, 0.2125)
        11 -> (0.40082222609352647, 0.5220066643438841, 0.85)
        12 -> (0.9728288904374106, 0.621644452187053, 0.07336199581899142)
        13 -> (0.736782672705901, 0.358, 0.5030266573755369)
        14 -> (0.28026441037696703, 0.715, 0.4292089322474965)
        _other -> error "modulus in mmaColor is broken"

moveToVec, lineToVec :: Vec2 -> Render ()
moveToVec (Vec2 x y) = moveTo x y
lineToVec (Vec2 x y) = lineTo x y

lineSketch :: Line -> Render ()
lineSketch (Line start end) = do
    moveToVec start
    lineToVec end

data ArrowSpec = ArrowSpec
    { arrowheadRelPos :: Distance
    , arrowheadSize   :: Distance
    , arrowDrawBody   :: Bool
    , arrowheadAngle  :: Angle
    }

instance Default ArrowSpec where
    def = ArrowSpec
        { arrowheadRelPos = Distance 1
        , arrowheadSize   = Distance 10
        , arrowDrawBody   = True
        , arrowheadAngle  = Angle 0.5
        }

arrowSketch :: Line -> ArrowSpec -> Render ()
arrowSketch line ArrowSpec{..} = do
    when arrowDrawBody (lineSketch line)

    let Line start end = line
        Angle rawLineAngle = angleOfLine line
        Angle rawArrowheadAngle = arrowheadAngle
        Distance rawRelPos = arrowheadRelPos

        arrowTip = start +. (rawRelPos *. (end -. start))

    let arrowheadHalf (+-) = angledLine arrowTip (Angle (rawLineAngle + pi +- rawArrowheadAngle)) arrowheadSize
        Line _ arrowLeftEnd  = arrowheadHalf (+)
        Line _ arrowRightEnd = arrowheadHalf (-)
    moveToVec arrowLeftEnd
    lineToVec arrowTip
    lineToVec arrowRightEnd

circleSketch :: Vec2 -> Distance -> Render ()
circleSketch (Vec2 x y) (Distance r) = arc x y r 0 (2*pi)

crossSketch :: Vec2 -> Distance -> Render ()
crossSketch center (Distance r) = do
    let lowerRight = rotateAround center (deg 45) (center +. Vec2 r 0)
        line1 = angledLine lowerRight (deg (45+180)) (Distance (2*r))
        line2 = rotateAround center (deg 90) line1
    lineSketch line1
    lineSketch line2

arcSketch :: Vec2 -> Distance -> Angle -> Angle -> Render ()
arcSketch (Vec2 x y) (Distance r) (Angle angleStart) (Angle angleEnd)
  = arc x y r angleStart angleEnd

polygonSketch :: Polygon -> Render ()
polygonSketch (Polygon []) = pure ()
polygonSketch (Polygon (Vec2 x y : vecs)) = do
    moveTo x y
    for_ vecs (\(Vec2 x' y') -> lineTo x' y')
    closePath

-- | Draw a caresian coordinate system in range (x,x') (y,y')
cartesianCoordinateSystem :: Render ()
cartesianCoordinateSystem = do
    let minMax :: (Int, Int)
        minMax = (-1000, 1000)
        (minX, maxX) = minMax
        (minY, maxY) = minMax
    let vec2 x y = Vec2 (fromIntegral x) (fromIntegral y)
    setLineWidth 1
    hsva 0 0 0 0.5
    sequence_ [ lineSketch (Line (vec2 x minY) (vec2 x maxY))
              | x <- [minX, minX+100 .. maxX] ]
    sequence_ [ lineSketch (Line (vec2 minX y) (vec2 maxX y))
              | y <- [minY, minY+100 .. maxY] ]
    stroke

    hsva 0 0 0 0.2
    sequence_ [ lineSketch (Line (vec2 x minY) (vec2 x maxY))
              | x <- [minX, minX+10 .. maxX]
              , mod x 100 /= 0 ]
    sequence_ [ lineSketch (Line (vec2 minX y) (vec2 maxX y))
              | y <- [minY, minY+10 .. maxY]
              , mod y 100 /= 0]
    stroke

    let centeredText :: Int -> Int -> String -> Render ()
        centeredText x y str = do
            TextExtents{textExtentsWidth = w, textExtentsHeight = h} <- textExtents str
            moveTo (fromIntegral x - w/2) (fromIntegral y + h)
            showText str
    setFontSize 8
    mmaColor 0 1
    sequence_ [ centeredText x y (show x ++ "," ++ show y)
              | x <- [minX, minX+100 .. maxX]
              , y <- [minY, minY+100 .. maxY] ]

radialCoordinateSystem :: Vec2 -> Int -> Render ()
radialCoordinateSystem center maxR = do
    let distance = Distance . fromIntegral
    setLineWidth 1
    hsva 0 0 0 1
    sequence_ [ circleSketch center (distance r) >> stroke
              | r <- [100, 200 .. maxR] ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) (distance maxR)) >> stroke
              | angle <- init [0, 45 .. 360 :: Int] ]

    hsva 0 0 0 0.5
    sequence_ [ circleSketch center (distance r) >> stroke
              | r <- [25, 50 .. maxR]
              , mod r 100 /= 0 ]
    sequence_ [ lineSketch (angledLine center (deg (fromIntegral angle)) (distance maxR)) >> stroke
              | angle <- init [0, 15 .. 360 :: Int]
              , mod angle 45 /= 0 ]

-- | Temporarily draw using a different operator. Useful e.g. to delete somthing
-- from the current drawing and then going on as before.
withOperator :: Operator -> Render a -> Render a
withOperator op render = do
    formerOp <- getOperator
    setOperator op
    result <- render
    setOperator formerOp
    pure result

-- | Render something with a new Cairo state, restoring the old one afterwards.
--
-- Handles the bookkeeping with 'save' and 'restore' internally, and can be used
-- to e.g. temporarily change the transformation matrix.
withSavedState :: Render a -> Render a
withSavedState render = do
    save
    result <- render
    restore
    pure result

-- | Render something as a group, as in encapsulate it in 'pushGroup' and
-- 'popGroupToSource'.
--
-- The second parameter can be used to specify an action to be run after
-- grouping, such as 'paintWithAlpha'.
grouped :: Render after -> Render a -> Render a
grouped afterwards render = do
    pushGroup
    result <- render
    popGroupToSource
    _ <- afterwards
    pure result

data VAlign = VTop | VCenter | VBottom
data HAlign = HLeft | HCenter | HRight

showTextAligned :: HAlign -> VAlign -> String -> Render ()
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
