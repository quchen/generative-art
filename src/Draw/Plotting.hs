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
    , previewCanvas
    , pause
    , PauseMode(..)
    , withFeedrate
    , withDrawingHeight

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



import Control.Monad.RWS
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
import Data.Maybe (fromMaybe)
import Util


newtype Plot a = Plot (RWS PlottingSettings ([GCode], BoundingBox) PlottingState a)
    deriving (Functor, Applicative, Monad, MonadReader PlottingSettings, MonadState PlottingState)

data PlottingState = PlottingState
    { _penState :: PenState
    , _penXY :: Vec2
    , _drawingDistance :: Double
    } deriving (Eq, Ord, Show)

data PenState = PenDown | PenUp deriving (Eq, Ord, Show)

data PlottingSettings = PlottingSettings
    { _feedrate :: Maybe Double
    -- ^ Either set a feedrate, or have an initial check whether one was set
    -- previously. ('def'ault: 'Nothing')

    , _zTravelHeight :: Double
    -- ^ During travel motion, keep the pen at this height (in absolute
    -- coordinates). ('def'ault: 1)

    , _zDrawingHeight :: Double
    -- ^ When drawing, keep the pen at this height (in absolute coordinates).
    -- ('def'ault: -1)

    , _finishMove :: Maybe FinishMove
    -- ^ Do a final move after the drawing has ended. ('def'ault: 'Nothing')

    , _previewDrawnShapesBoundingBox :: Bool
    -- ^ At the beginning of the plot, trace the bounding box of all the GCode
    -- before actually drawing? Useful as a final check. ('def'ault: 'True')

    , _canvasBoundingBox :: Maybe BoundingBox
    -- ^ The canvas we’re painting on. Useful to check whether the pen leaves
    -- the drawing area. ('def'ault: 'Nothing')
    } deriving (Eq, Ord, Show)

-- | Command to issue in the footer
data FinishMove = FinishWithG28 | FinishWithG30
    deriving (Eq, Ord, Show)

instance Default PlottingSettings where
    def = PlottingSettings
        { _feedrate = Nothing
        , _zTravelHeight = 1
        , _zDrawingHeight = -1
        , _finishMove = Nothing
        , _previewDrawnShapesBoundingBox = True
        , _canvasBoundingBox = Nothing
        }

-- | Add raw GCode to the output.
gCode :: [GCode] -> Plot ()
gCode instructions = for_ instructions $ \instruction -> do
    Plot (tell ([instruction], mempty))
    recordDrawingDistance instruction
    recordPenXY instruction
    recordBoundingBox instruction
  where
    setPenXY :: Vec2 -> Plot ()
    setPenXY pos = do
        asks _canvasBoundingBox >>= \case
            Just bb
                -- NB: This works for straight lines, but misses arcs that move outside the
                -- plotting area and back inside, possible with e.g. arc interpolation
                | not (insideBoundingBox pos bb) -> error "Tried to move pen outside the plotting area!"
            _otherwise -> pure ()
        modify' (\s -> s { _penXY = pos })


    recordPenXY :: GCode -> Plot ()
    recordPenXY instruction = do
        Vec2 x0 y0 <- gets _penXY
        case instruction of
            G00_LinearRapidMove x y _         -> setPenXY (Vec2 (fromMaybe x0 x) (fromMaybe y0 y))
            G01_LinearFeedrateMove _ x y _    -> setPenXY (Vec2 (fromMaybe x0 x) (fromMaybe y0 y))
            G02_ArcClockwise _ _ _ x y        -> setPenXY (Vec2 x y)
            G03_ArcCounterClockwise _ _ _ x y -> setPenXY (Vec2 x y)
            _otherwise                        -> pure ()

    recordDrawingDistance :: GCode -> Plot ()
    recordDrawingDistance instruction = do
        penState <- gets _penState
        penXY@(Vec2 x0 y0) <- gets _penXY
        when (penState == PenDown) $ case instruction of
            G00_LinearRapidMove x y _      -> addDrawingDistance (norm (penXY -. Vec2 (fromMaybe x0 x) (fromMaybe y0 y)))
            G01_LinearFeedrateMove _ x y _ -> addDrawingDistance (norm (penXY -. Vec2 (fromMaybe x0 x) (fromMaybe y0 y)))
            G02_ArcClockwise _ i j x y -> do
                let r = norm (Vec2 i j)
                    center = penXY +. Vec2 i j
                    angle = angleBetween (Line center penXY) (Line center (Vec2 x y))
                addDrawingDistance (r * getRad angle)
            G03_ArcCounterClockwise _ i j x y -> do
                let r = norm (Vec2 i j)
                    center = penXY +. Vec2 i j
                    angle = angleBetween (Line center penXY) (Line center (Vec2 x y))
                addDrawingDistance (r * getRad angle)
            _otherwise -> pure ()

    tellBB :: HasBoundingBox object => object -> Plot ()
    tellBB object = Plot (tell (mempty, boundingBox object))

    recordBoundingBox :: GCode -> Plot ()
    recordBoundingBox instruction = do
        penState <- gets _penState
        current@(Vec2 xCurrent yCurrent) <- gets _penXY
        when (penState == PenDown) $ case instruction of
            G00_LinearRapidMove x y _      -> tellBB (Vec2 (fromMaybe xCurrent x) (fromMaybe yCurrent y))
            G01_LinearFeedrateMove _ x y _ -> tellBB (Vec2 (fromMaybe xCurrent x) (fromMaybe yCurrent y))
            G02_ArcClockwise _ i j x y        -> tellBB (CwArc  current (Vec2 i j) (Vec2 x y))
            G03_ArcCounterClockwise _ i j x y -> tellBB (CcwArc current (Vec2 i j) (Vec2 x y))
            _otherwise -> pure ()

    addDrawingDistance :: Double -> Plot ()
    addDrawingDistance d = modify (\s -> s { _drawingDistance = _drawingDistance s + d })

-- | CwArc a r b = Clockwise arc from a to b with center at a+r.
data CwArc = CwArc Vec2 Vec2 Vec2 deriving (Eq, Ord, Show)

-- | CcwArc a r b = Counterclockwise arc from a to b with center at a+r.
data CcwArc = CcwArc Vec2 Vec2 Vec2 deriving (Eq, Ord, Show)

instance HasBoundingBox CwArc where
    boundingBox (CwArc start centerOffset end) =
        let radius = norm centerOffset
            center = start +. centerOffset

            startQuadrant = whichQuadrant (negateV centerOffset)
            endQuadrant = whichQuadrant (end -. start -. centerOffset)
            traversedQuadrants = cwQuadrantsFromTo startQuadrant endQuadrant

            quadrantTransitionPoints (QuadrantBR : rest@(QuadrantBL : _)) = (center +. Vec2 0 radius) : quadrantTransitionPoints rest
            quadrantTransitionPoints (QuadrantBL : rest@(QuadrantUL : _)) = (center -. Vec2 radius 0) : quadrantTransitionPoints rest
            quadrantTransitionPoints (QuadrantUL : rest@(QuadrantUR : _)) = (center -. Vec2 0 radius) : quadrantTransitionPoints rest
            quadrantTransitionPoints (QuadrantUR : rest@(QuadrantBR : _)) = (center +. Vec2 radius 0) : quadrantTransitionPoints rest
            quadrantTransitionPoints (q1:q2:_) = bugError "quadrantTransitionPoints" (show q2 ++ " is not after " ++ show q1 ++ " when traversing quadrants clockwise")
            quadrantTransitionPoints _ = []

        in boundingBox (start : end : quadrantTransitionPoints traversedQuadrants)

instance HasBoundingBox CcwArc where
    boundingBox (CcwArc start centerOffset end) =
        let radius = norm centerOffset
            center = start +. centerOffset

            startQuadrant = whichQuadrant (negateV centerOffset)
            endQuadrant = whichQuadrant (end -. start -. centerOffset)
            traversedQuadrants = ccwQuadrantsFromTo startQuadrant endQuadrant

            quadrantTransitionPoints (QuadrantBL : rest@(QuadrantBR : _)) = (center +. Vec2 0 radius) : quadrantTransitionPoints rest
            quadrantTransitionPoints (QuadrantUL : rest@(QuadrantBL : _)) = (center -. Vec2 radius 0) : quadrantTransitionPoints rest
            quadrantTransitionPoints (QuadrantUR : rest@(QuadrantUL : _)) = (center -. Vec2 0 radius) : quadrantTransitionPoints rest
            quadrantTransitionPoints (QuadrantBR : rest@(QuadrantUR : _)) = (center +. Vec2 radius 0) : quadrantTransitionPoints rest
            quadrantTransitionPoints (q1:q2:_) = bugError "quadrantTransitionPoints" (show q2 ++ " is not after " ++ show q1 ++ " when traversing quadrants counterclockwise")
            quadrantTransitionPoints _ = []

        in boundingBox (start : end : quadrantTransitionPoints traversedQuadrants)

data Quadrant = QuadrantBR | QuadrantBL | QuadrantUL | QuadrantUR deriving (Eq, Ord, Show)

-- | Quadrants are in Cairo coordinates (y pointing downwards!)
cwQuadrantsFromTo :: Quadrant -> Quadrant -> [Quadrant]
cwQuadrantsFromTo start end | start == end = [start]
cwQuadrantsFromTo start end = start : cwQuadrantsFromTo (cwNextQuadrant start) end

-- | Quadrants are in Cairo coordinates (y pointing downwards!)
ccwQuadrantsFromTo :: Quadrant -> Quadrant -> [Quadrant]
ccwQuadrantsFromTo start end | start == end = [start]
ccwQuadrantsFromTo start end = start : ccwQuadrantsFromTo (ccwNextQuadrant start) end

-- | Quadrants are in Cairo coordinates (y pointing downwards!)
cwNextQuadrant :: Quadrant -> Quadrant
cwNextQuadrant QuadrantBR = QuadrantBL
cwNextQuadrant QuadrantBL = QuadrantUL
cwNextQuadrant QuadrantUL = QuadrantUR
cwNextQuadrant QuadrantUR = QuadrantBR

-- | Quadrants are in Cairo coordinates (y pointing downwards!)
ccwNextQuadrant :: Quadrant -> Quadrant
ccwNextQuadrant QuadrantBL = QuadrantBR
ccwNextQuadrant QuadrantUL = QuadrantBL
ccwNextQuadrant QuadrantUR = QuadrantUL
ccwNextQuadrant QuadrantBR = QuadrantUR

-- | Quadrants are in Cairo coordinates (y pointing downwards!)
whichQuadrant :: Vec2 -> Quadrant
whichQuadrant (Vec2 x y)
    | x >= 0 && y >= 0 = QuadrantBR
    | x <  0 && y >= 0 = QuadrantBL
    | x <  0 && y <  0 = QuadrantUL
    | otherwise        = QuadrantUR

-- | Trace the plotting area to preview the extents of the plot, and wait for
-- confirmation. Useful at the start of a plot.
previewCanvas :: Plot ()
previewCanvas = do
    comment "Preview bounding box"
    asks _canvasBoundingBox >>= \case
        Just bb -> plot bb >> pause PauseUserConfirm
        Nothing -> pure ()

-- | Quick move for repositioning (without drawing).
repositionTo :: Vec2 -> Plot ()
repositionTo target@(Vec2 x y) = do
    currentXY <- gets _penXY
    when (currentXY /= target) $ do
        penUp
        gCode [ G00_LinearRapidMove (Just x) (Just y) Nothing ]

-- | Draw a line from the current position to a target.
lineTo :: Vec2 -> Plot ()
lineTo target@(Vec2 x y) = do
    currentXY <- gets _penXY
    feedrate <- asks _feedrate
    when (currentXY /= target) $ do
        penDown
        gCode [ G01_LinearFeedrateMove feedrate (Just x) (Just y) Nothing ]

-- | Arc interpolation, clockwise
clockwiseArcAroundTo
    :: Vec2 -- ^ Center location, relative to the current pen position
    -> Vec2 -- ^ End position
    -> Plot ()
clockwiseArcAroundTo (Vec2 mx my) (Vec2 x y) = do
    feedrate <- asks _feedrate
    penDown
    gCode [ G02_ArcClockwise feedrate mx my x y ]

-- | Arc interpolation, counterclockwise
counterclockwiseArcAroundTo
    :: Vec2 -- ^ Center location, relative to the current pen position
    -> Vec2 -- ^ End position
    -> Plot ()
counterclockwiseArcAroundTo (Vec2 mx my) (Vec2 x y) = do
    feedrate <- asks _feedrate
    penDown
    gCode [ G03_ArcCounterClockwise feedrate mx my x y ]

-- | If the pen is up, lower it to drawing height. Do nothing if it is already
-- lowered.
penDown :: Plot ()
penDown = gets _penState >>= \case
    PenDown -> pure ()
    PenUp -> do
        zDrawing <- asks _zDrawingHeight
        gCode [ G00_LinearRapidMove Nothing Nothing (Just zDrawing) ]
        modify (\s -> s { _penState = PenDown })

-- | If the pen is down, lift it to travel height. Do nothing if it is already
-- lifted.
penUp :: Plot ()
penUp = gets _penState >>= \case
    PenUp -> pure ()
    PenDown -> do
        zTravel <- asks _zTravelHeight
        gCode [ G00_LinearRapidMove Nothing Nothing (Just zTravel) ]
        modify (\s -> s { _penState = PenUp })

-- | Locally change the feedrate
withFeedrate :: Double -> Plot a -> Plot a
withFeedrate f = local (\settings -> settings { _feedrate = Just f })

-- | Locally adapt the z drawing height (e.g. for changing pen pressure)
withDrawingHeight :: Double -> Plot a -> Plot a
withDrawingHeight z = local (\settings -> settings { _zDrawingHeight = z })

block :: Plot a -> Plot a
block (Plot content) = Plot (mapRWS (\(a, s, (gcode, bb)) -> (a, s, ([GBlock gcode], bb))) content)

-- | Add a GCode comment
comment :: TL.Text -> Plot ()
comment txt = gCode [ GComment txt ]

-- | Pause the plot for later resumption at the current state
pause :: PauseMode -> Plot ()
pause PauseUserConfirm = penUp >> gCode [ M0_Pause ]
pause (PauseSeconds seconds) = gCode [ G04_Dwell seconds ]

data PauseMode
    = PauseUserConfirm -- ^ Wait until user confirmation (in e.g. a web UI or with a button)
    | PauseSeconds Double -- ^ Wait for a certain time
    deriving (Eq, Ord, Show)

addHeaderFooter :: Maybe feedrate -> Maybe FinishMove -> Maybe (BoundingBox, Double) -> [GCode] -> [GCode]
addHeaderFooter feedrate finishMove drawnShapesBoundingBox body = header : body ++ [footer]
  where
    feedrateCheck = case feedrate of
        Just _ -> GBlock []
        Nothing -> GBlock
            [ GComment "NOOP move to make sure feedrate is already set externally"
            , G91_RelativeMovement
            , G01_LinearFeedrateMove Nothing (Just 0) (Just 0) (Just 0)
            , G90_AbsoluteMovement
            ]

    boundingBoxCheck = case drawnShapesBoundingBox of
        Nothing -> GBlock []
        Just (BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax), zTravelHeight) -> GBlock $
            GComment "Trace GCode bounding box"
            : intersperse (G04_Dwell 0.5)
                [ G00_LinearRapidMove (Just x) (Just y) (Just zTravelHeight)
                | Vec2 x y <- [ Vec2 xMin yMin
                              , Vec2 xMax yMin
                              , Vec2 xMax yMax
                              , Vec2 xMin yMax
                              , Vec2 xMin yMin] ]

    header = GBlock
        [ GComment "Header"
        , feedrateCheck
        , boundingBoxCheck
        ]

    footer = GBlock
        [ GComment "Footer"
        , finishMoveCheck
        ]

    finishMoveCheck = case finishMove of
        Nothing -> GBlock
            [ GComment "Lift pen"
            , G00_LinearRapidMove Nothing Nothing (Just 10)
            ]
        Just FinishWithG28 -> GBlock
            [ GComment "Move to predefined position"
            , G28_GotoPredefinedPosition Nothing Nothing (Just 10)
            ]
        Just FinishWithG30 -> GBlock
            [ GComment "Move to predefined position"
            , G30_GotoPredefinedPosition Nothing Nothing (Just 10)
            ]

runPlot :: PlottingSettings -> Plot a -> TL.Text
runPlot settings body =
    let (_, _finalState, (gcode, drawnBB)) = runRWS body' settings initialState
    in renderGCode
        (addHeaderFooter
            (_feedrate settings)
            (_finishMove settings)
            (if _previewDrawnShapesBoundingBox settings then Just (drawnBB, _zTravelHeight settings) else Nothing)
            gcode)
  where
    Plot body' = body
    initialState = PlottingState
        { _penState = PenUp
        , _penXY = Vec2 (1/0) (1/0) -- Nonsense value so we’re always misaligned in the beginning, making every move command actually move
        , _drawingDistance = 0
        }

-- | Draw a shape by lowering the pen, setting the right speed, etc. The specifics
-- are defined in the configuration given in 'runPlot', or by the various utility
-- functions such as 'withFeedrate' or 'withDrawingHeight'
class Plotting a where
    plot :: a -> Plot ()

-- | Trace the bounding box without actually drawing anything to estimate result size
instance Plotting BoundingBox where
    plot bb = block $ do
        comment "Hover over bounding box"
        plot (boundingBoxPolygon bb)

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

-- | Draw each element (in order)
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
