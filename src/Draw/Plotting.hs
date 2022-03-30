{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw.Plotting (
    -- * 'Plot' type
      Plot()
    , PlottingSettings(..)
    , runPlot

    -- * 'Plotting' shapes
    , Plotting(..)

    -- * Plotting primitives
    , repositionTo
    , lineTo
    , clockwiseArcAroundTo
    , counterclockwiseArcAroundTo
    , setFeedrate
    , pause

    -- ** File structure
    , block
    , comment

    -- * Raw G-Code
    , penDown
    , penUp
    , gCode

    -- * Utilities
    , minimizePenHovering
) where



import Control.Monad.State
import Control.Monad.Writer
import Data.Default.Class
import Data.List
import           Data.Foldable
import qualified Data.Set       as S
import qualified Data.Text.Lazy as TL
import           Data.Vector    (Vector)
import qualified Data.Vector    as V

import Draw.Plotting.GCode
import Geometry.Bezier
import Geometry.Core
import Geometry.Shapes


newtype Plot a = Plot (StateT PlottingState (Writer [GCode]) a)
    deriving (Functor, Applicative, Monad, MonadWriter [GCode], MonadState PlottingState)

data PlottingState = PlottingState
    { _plottingSettings :: PlottingSettings
    , _penState :: PenState
    , _penXY :: Vec2
    } deriving (Eq, Ord, Show)

data PenState = PenDown | PenUp deriving (Eq, Ord, Show)

data PlottingSettings = PlottingSettings
    { _previewBoundingBox :: Maybe BoundingBox
    -- ^ Before plotting, trace this bounding box to preview extents of the plot
    --   and wait for confirmation. ('def'ault: 'Nothing)

    , _feedrate :: Maybe Double
    -- ^ Either set a feedrate, or have an initial check whether one was set
    -- previously. ('def'ault: 'Nothing)

    , _zTravelHeight :: Double
    -- ^ During travel motion, keep the pen at this height (in absolute
    -- coordinates). ('def'ault: 1)

    , _zDrawingHeight :: Double
    -- ^ When drawing, keep the pen at this height (in absolute coordinates).
    -- ('def'ault: -1)

    , _finishMove :: Maybe FinishMove
    -- ^ Do a final move after the drawing has ended
    } deriving (Eq, Ord, Show)

-- | Command to issue in the footer
data FinishMove = FinishWithG28 | FinishWithG30
    deriving (Eq, Ord, Show)

instance Default PlottingSettings where
    def = PlottingSettings
        { _previewBoundingBox = Nothing
        , _feedrate = Nothing
        , _zTravelHeight = 1
        , _zDrawingHeight = -1
        , _finishMove = Nothing
        }

-- | Add raw GCode to the output.
gCode :: [GCode] -> Plot ()
gCode = tell

setPenXY :: Vec2 -> Plot ()
setPenXY pos = modify' (\s -> s { _penXY = pos })

-- | Quick move for repositioning (without drawing).
repositionTo :: Vec2 -> Plot ()
repositionTo target@(Vec2 x y) = do
    currentXY <- gets _penXY
    when (currentXY /= target) $ do
        penUp
        gCode [ G00_LinearRapidMove (Just x) (Just y) Nothing ]
        setPenXY target

-- | Draw a line from the current position to a target.
lineTo :: Vec2 -> Plot ()
lineTo target@(Vec2 x y) = do
    currentXY <- gets _penXY
    when (currentXY /= target) $ do
        penDown
        gCode [ G01_LinearFeedrateMove (Just x) (Just y) Nothing ]
        setPenXY target

-- | Center is always given in _relative_ coordinates, but target in G90 (absolute) or G91 (relative) coordinates!
counterclockwiseArcAroundTo :: Vec2 -> Vec2 -> Plot ()
counterclockwiseArcAroundTo (Vec2 mx my) target@(Vec2 x y) = do
    penDown
    gCode [ G03_ArcCounterClockwise mx my x y ]
    setPenXY target

-- | Center is always given in _relative_ coordinates, but target in G90 (absolute) or G91 (relative) coordinates!
clockwiseArcAroundTo :: Vec2 -> Vec2 -> Plot ()
clockwiseArcAroundTo (Vec2 mx my) target@(Vec2 x y) = do
    penDown
    gCode [ G02_ArcClockwise mx my x y ]
    setPenXY target

-- | If the pen is up, lower it to drawing height. Do nothing if it is already
-- lowered.
penDown :: Plot ()
penDown = gets _penState >>= \case
    PenDown -> pure ()
    PenUp -> do
        zDrawing <- gets (_zDrawingHeight . _plottingSettings)
        gCode [ G00_LinearRapidMove Nothing Nothing (Just zDrawing) ]
        modify (\s -> s { _penState = PenDown })

-- | If the pen is down, lift it to travel height. Do nothing if it is already
-- lifted.
penUp :: Plot ()
penUp = gets _penState >>= \case
    PenUp -> pure ()
    PenDown -> do
        zTravel <- gets (_zTravelHeight . _plottingSettings)
        gCode [ G00_LinearRapidMove Nothing Nothing (Just zTravel) ]
        modify (\s -> s { _penState = PenUp })

setFeedrate :: Double -> Plot ()
setFeedrate f = gCode [ F_Feedrate f ]

block :: Plot a -> Plot a
block (Plot content) = Plot (mapStateT (mapWriter (\(a, g) -> (a, [GBlock g]))) content) <* penUp -- for extra safety

comment :: TL.Text -> Plot ()
comment txt = gCode [ GComment txt ]

pause :: PauseMode -> Plot ()
pause PauseUserConfirm = penUp >> gCode [ M0_Pause ]
pause (PauseSeconds seconds) = gCode [ G04_Dwell seconds ]

data PauseMode = PauseUserConfirm | PauseSeconds Double deriving (Eq, Ord, Show)

addHeaderFooter :: Plot a -> Plot a
addHeaderFooter body = block $ do
    header
    a <- body
    footer
    pure a
  where
    feedrateCheck = gets (_feedrate . _plottingSettings) >>= \case
        Just f -> comment "Initial feedrate" >> setFeedrate f
        Nothing -> gCode
            [ GComment "NOOP move to make sure feedrate is already set externally"
            , G91_RelativeMovement
            , G01_LinearFeedrateMove (Just 0) (Just 0) (Just 0)
            , G90_AbsoluteMovement
            ]

    previewBoundingBox = gets (_previewBoundingBox . _plottingSettings) >>= \case
        Just bb -> do
            comment "Preview bounding box"
            plot bb
            pause PauseUserConfirm
        Nothing -> pure ()

    header = block $ do
        comment "Header"
        feedrateCheck
        previewBoundingBox

    footer = block $ do
        comment "Footer"
        gets (_finishMove . _plottingSettings) >>= \case
            Nothing -> do
                comment "Lift pen"
                gCode [ G00_LinearRapidMove Nothing Nothing (Just 10) ]
            Just FinishWithG28 -> do
                comment "Move to predefined position"
                gCode [ G28_GotoPredefinedPosition Nothing Nothing (Just 10) ]
            Just FinishWithG30 -> do
                comment "Move to predefined position"
                gCode [ G30_GotoPredefinedPosition Nothing Nothing (Just 10) ]

runPlot :: PlottingSettings -> Plot a -> TL.Text
runPlot settings body = renderGCode (execWriter (evalStateT finalPlot initialState))
  where
    Plot finalPlot = addHeaderFooter body
    initialState = PlottingState
        { _plottingSettings = settings
        , _penState = PenUp
        , _penXY = Vec2 (1/0) (1/0) -- Nonsense value so we’re always misaligned in the beginning, making every move command actually move
        }

class Plotting a where
    plot :: a -> Plot ()

-- | Trace the bounding box without actually drawing anything to estimate result size
instance Plotting BoundingBox where
    plot (BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax)) = block $ do
        comment "Hover over bounding box"
        sequence_ . intersperse (pause (PauseSeconds 0.5)) . map repositionTo $
            [ Vec2 xMin yMin
            , Vec2 xMax yMin
            , Vec2 xMax yMax
            , Vec2 xMin yMax
            , Vec2 xMin yMin
            ]

instance Plotting Line where
    plot (Line start end) = block $ do
        comment "Line"
        repositionTo start
        lineTo end

instance Plotting Circle where
    plot (Circle center radius) = block $ do
        comment "Circle"

        -- The naive way of painting a circle is by always starting them e.g. on
        -- the very left. This requires some unnecessary pen hovering, and for some
        -- pens creates a visible »pen down« dot. We therefore go the more
        -- complicated route here: start the circle at the point closest to the pen
        -- position.
        currentXY <- gets _penXY
        let radialLine@(Line _ start) = resizeLine (const radius) (Line center currentXY)
            Line _ oppositeOfStart = resizeLine negate radialLine

        repositionTo start
        -- FluidNC 3.4.2 has a bug where small circles (2mm radius) sometimes don’t do
        -- anything when we plot it with a single arc »from start to itself«. We work
        -- around this by explicitly chaining two half circles.
        clockwiseArcAroundTo (vectorOf (lineReverse radialLine)) oppositeOfStart
        clockwiseArcAroundTo (vectorOf radialLine) start

-- | Approximation by a number of points
instance Plotting Ellipse where
    plot (Ellipse trafo) = block $ do
        comment "Ellipse"
        plot (transform trafo (regularPolygon 64))

instance Foldable f => Plotting (Polyline f) where
    plot (Polyline xs) = go (toList xs)
      where
        go [] = pure ()
        go (p:ps) = block $ do
            comment "Polyline"
            repositionTo p
            traverse_ lineTo ps

instance (Functor f, Sequential f, Plotting a) => Plotting (f a) where
    plot x = block $ do
        comment "Sequential"
        traverse_ plot x

-- | Draw each element (in order)
instance (Plotting a, Plotting b) => Plotting (a,b) where
    plot (a,b) = block $ do
        comment "2-tuple"
        plot a
        plot b

-- | Draw each element (in order)
instance (Plotting a, Plotting b, Plotting c) => Plotting (a,b,c) where
    plot (a,b,c) = block $ do
        comment "3-tuple"
        plot a
        plot b
        plot c

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (Plotting a, Plotting b, Plotting c, Plotting d) => Plotting (a,b,c,d) where
    plot (a,b,c,d) = block $ do
        comment "4-tuple"
        plot a
        plot b
        plot c
        plot d

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (Plotting a, Plotting b, Plotting c, Plotting d, Plotting e) => Plotting (a,b,c,d,e) where
    plot (a,b,c,d,e) = block $ do
        comment "5-tuple"
        plot a
        plot b
        plot c
        plot d
        plot e

instance Plotting Polygon where
    plot (Polygon []) = pure ()
    plot (Polygon (p:ps)) = block $ do -- Like polyline, but closes up the shape
        comment "Polygon"
        repositionTo p
        traverse_ lineTo ps
        lineTo p

-- | FluidNC doesn’t support G05, so we approximate Bezier curves with line pieces.
-- We use the naive Bezier interpolation 'bezierSubdivideT', because it just so
-- happens to put more points in places with more curvature.
instance Plotting Bezier where
    plot bezier@(Bezier a _ _ _) = block $ do
        comment "Bezier (cubic)"
        repositionTo a
        traverse_ lineTo (bezierSubdivideT 32 bezier)

minimumOn :: (Foldable f, Ord ord) => (a -> ord) -> f a -> Maybe a
minimumOn f xs
    | null xs = Nothing
    | otherwise = Just (minimumBy (\x y -> compare (f x) (f y)) xs)

-- | Sort a collection of polylines so that between each line pair, we only do the shortest move.
-- This is a local solution to what would be TSP if solved globally. Better than nothing I guess,
-- although this algorithm here is \(\mathcal O(n^2)\).
minimizePenHovering :: Sequential vector => S.Set (vector Vec2) -> [Vector Vec2]
minimizePenHovering = mergeStep . sortStep (Vec2 0 0) . S.map toVector
  where
    -- Sort by minimal travel between adjacent lines
    sortStep :: Vec2 -> S.Set (Vector Vec2) -> [Vector Vec2]
    sortStep penPos pool =
        let closestNextLine = minimumOn (\candidate -> norm (V.head candidate -. penPos) `min` norm (V.last candidate -. penPos)) pool
        in case closestNextLine of
            Nothing -> []
            Just l ->
                let rightWayRound = if norm (V.head l -. penPos) > norm (V.last l -. penPos)
                        then V.reverse l
                        else l
                    remainingPool = S.delete l pool
                    newPenPos = V.last rightWayRound
                in rightWayRound : sortStep newPenPos remainingPool

    -- Merge adjacent polylines
    mergeStep :: [Vector Vec2] -> [Vector Vec2]
    mergeStep (t1:t2:rest) = case (V.unsnoc t1, V.uncons t2) of
        (Just (_t1Init, t1Last), Just (t2Head, t2Tail))
            | t1Last == t2Head -> mergeStep (t1 <> t2Tail:rest)
        _ -> t1 : mergeStep (t2:rest)
    mergeStep other = other
