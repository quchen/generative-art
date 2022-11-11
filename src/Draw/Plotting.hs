{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Draw.Plotting (
    -- * 'Plot' type
      Plot()
    , runPlot
    , GCode()
    , writeGCodeFile
    , renderPreview
    , RunPlotResult(..)
    , PlottingSettings(..)
    , FinishMove(..)

    -- ** Raw GCode handling
    , TinkeringInternals(..)
    , PlottingWriterLog(..)
    , PlottingState(..)
    , renderGCode

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
    , drawingDistance

    -- ** File structure
    , block
    , comment

    -- * Raw G-Code
    , penDown
    , penUp
    , gCode

    -- * Utilities
    , minimizePenHovering
    , minimizePenHoveringBy
    , MinimizePenHoveringSettings(..)
    , module Data.Default.Class
) where



import           Control.Monad.RWS        hiding (modify)
import           Data.DList               (DList)
import qualified Data.DList               as DL
import           Data.Default.Class
import           Data.Foldable
import           Data.Maybe
import qualified Data.Set                 as S
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy.IO        as TL
import           Data.Vector              (Vector)
import qualified Data.Vector              as V
import           Formatting               hiding (center)
import qualified Graphics.Rendering.Cairo as C hiding (x, y)

import qualified Draw                as D
import           Draw.Plotting.GCode
import           Geometry.Bezier
import           Geometry.Core
import           Geometry.Shapes



-- | 'Plot' represents penplotting directives, and is manipulated using functions
-- such as 'plot' and 'gCode'.
newtype Plot a = Plot (RWS PlottingSettings PlottingWriterLog PlottingState a)
    deriving (Functor, Applicative, Monad, MonadReader PlottingSettings, MonadState PlottingState)

data PlottingWriterLog = PlottingWriterLog
    { _plottedGCode :: DList GCode
    , _penTravelDistance :: !Double
    , _elementsDrawn :: !Int
    , _plottingCairoPreview :: C.Render ()
    }

instance Semigroup PlottingWriterLog where
    PlottingWriterLog code1 travel1 numDrawn1 render1 <> PlottingWriterLog code2 travel2 numDrawn2 render2
      = PlottingWriterLog (code1 <> code2) (travel1 + travel2) (numDrawn1 + numDrawn2) (render1 >> render2)

instance Monoid PlottingWriterLog where
    mempty = PlottingWriterLog mempty 0 0 (pure ())

{-# DEPRECATED modify "Use modify'. There’s no reason to lazily update the state." #-}
modify, _don'tReportModifyAsUnused :: a
modify = error "Use modify'. There’s no reason to lazily update the state."
_don'tReportModifyAsUnused = modify

data PlottingState = PlottingState
    { _penState :: !PenState
    , _penXY :: !Vec2
    , _drawingDistance :: !Double
    , _drawnBoundingBox :: !BoundingBox
    } deriving (Eq, Ord, Show)

data PenState = PenDown | PenUp deriving (Eq, Ord, Show)

data PlottingSettings = PlottingSettings
    { _feedrate :: Double
    -- ^ Initial feedrate. Can be modified locally with 'withFeedrate'. ('def'ault: 1000)

    , _zTravelHeight :: Double
    -- ^ During travel motion, keep the pen at this height (in absolute
    -- coordinates). ('def'ault: 1)

    , _zDrawingHeight :: Double
    -- ^ When drawing, keep the pen at this height (in absolute coordinates).
    -- ('def'ault: -1)

    , _zLoweringFeedrate :: Maybe Double
    -- ^ Use this feedrate for lowering the pen. On fast machines, lowering it
    -- at max speed might lead to unwanted vibrations. 'Nothing' means as fast
    -- as possible. ('def'ault: 'Nothing')

    , _finishMove :: Maybe FinishMove
    -- ^ Do a final move after the drawing has ended. ('def'ault: 'Nothing')

    , _previewDrawnShapesBoundingBox :: Bool
    -- ^ At the beginning of the plot, trace the bounding box of all the GCode
    -- before actually drawing? Useful as a final check. ('def'ault: 'True')

    , _canvasBoundingBox :: Maybe BoundingBox
    -- ^ The canvas we’re painting on. Useful to check whether the pen leaves
    -- the drawing area. ('def'ault: 'Nothing')

    , _previewPenWidth :: Double
    -- ^ Use this line width in the preview. To get a realistic preview, match
    -- this value with the actual stroke width of your pen. ('def'ault: 1)

    , _previewPenColor :: D.Color Double
    -- ^ Use this color for drawings in the preview. To get a realistic preview,
    -- match this value with the actual color of your pen.
    -- ('def'ault: @'mathematica97' 1@)

    , _previewPenTravelColor :: Maybe (D.Color Double)
    -- ^ Use this color for indicating pen travel in the preview. 'Nothing'
    -- will disable pen travel preview. ('def'ault: @'Just' ('mathematica97' 0)@)

    , _previewDecorate :: Bool
    -- ^ Show additional decoration in the preview, like origin and bounding box.
    -- ('def'ault: 'True')
    } deriving (Eq, Show)

-- | Command to issue in the footer
data FinishMove = FinishWithG28 | FinishWithG30
    deriving (Eq, Ord, Show)

instance Default PlottingSettings where
    def = PlottingSettings
        { _feedrate = 1000
        , _zTravelHeight = 1
        , _zDrawingHeight = -1
        , _zLoweringFeedrate = Nothing
        , _finishMove = Nothing
        , _previewDrawnShapesBoundingBox = True
        , _canvasBoundingBox = Nothing
        , _previewPenWidth = 1
        , _previewPenColor = D.mathematica97 0
        , _previewPenTravelColor = Just (D.mathematica97 1)
        , _previewDecorate = True
        }

-- | Add raw GCode to the output.
gCode :: [GCode] -> Plot ()
gCode instructions = for_ instructions $ \instruction -> do
    Plot (tell mempty{_plottedGCode = DL.singleton instruction})
    recordDrawingDistance instruction
    recordCairoPreview instruction
    recordBoundingBox instruction
    checkPlotDoesNotLeaveCanvas
    recordPenXY instruction -- NB: this is last because the other recorders depend on the pen position!

setPenXY :: Vec2 -> Plot ()
setPenXY pos = modify' (\s -> s { _penXY = pos })

checkPlotDoesNotLeaveCanvas :: Plot ()
checkPlotDoesNotLeaveCanvas = asks _canvasBoundingBox >>= \case
    Nothing -> pure ()
    Just canvasBB -> do
        drawnBB <- gets _drawnBoundingBox
        unless (drawnBB `insideBoundingBox` canvasBB) (error "Tried to move pen outside the canvas!")

recordPenXY :: GCode -> Plot ()
recordPenXY instruction = do
    Vec2 x0 y0 <- gets _penXY
    case instruction of
        G00_LinearRapidMove x y _         -> setPenXY (Vec2 (fromMaybe x0 x) (fromMaybe y0 y))
        G01_LinearFeedrateMove _ x y _    -> setPenXY (Vec2 (fromMaybe x0 x) (fromMaybe y0 y))
        G02_ArcClockwise _ _ _ x y        -> setPenXY (Vec2 x y)
        G03_ArcCounterClockwise _ _ _ x y -> setPenXY (Vec2 x y)
        _otherwise -> pure ()

tellCairo :: C.Render () -> Plot ()
tellCairo c = Plot (tell mempty{_plottingCairoPreview = D.cairoScope c})

recordCairoPreview :: GCode -> Plot ()
recordCairoPreview instruction = do
    start@(Vec2 currentX currentY) <- gets _penXY
    penState <- gets _penState
    settings <- ask
    when (not (isInfinite currentX) && not (isInfinite currentY) && (penState == PenDown || isJust (_previewPenTravelColor settings))) $ do
        let paintStyle = case penState of
                PenUp -> D.setColor (fromJust (_previewPenTravelColor settings))
                PenDown -> D.setColor (_previewPenColor settings)
            fastStyle = C.setLineWidth (_previewPenWidth settings) >> C.setDash [1,1] 0
            feedrateStyle = C.setLineWidth (_previewPenWidth settings)
        case instruction of
            G00_LinearRapidMove x y _ -> tellCairo $ do
                fastStyle
                paintStyle
                let end = Vec2 (fromMaybe currentX x) (fromMaybe currentY y)
                D.sketch (Line start end)
                C.stroke
            G01_LinearFeedrateMove _ x y _ -> tellCairo $ do
                feedrateStyle
                paintStyle
                let end = Vec2 (fromMaybe currentX x) (fromMaybe currentY y)
                D.sketch (Line start end)
                C.stroke
            G02_ArcClockwise _ i j x y -> tellCairo $ do
                feedrateStyle
                paintStyle
                let radius = norm centerOffset
                    centerOffset = Vec2 i j
                    center@(Vec2 centerX centerY) = start +. centerOffset
                    end = Vec2 x y
                    startAngle = angleOfLine (Line center start)
                    endAngle = angleOfLine (Line center end)
                D.moveToVec start
                C.arcNegative centerX centerY radius (getRad startAngle) (getRad endAngle)
                C.stroke
            G03_ArcCounterClockwise _ i j x y -> tellCairo $ do
                feedrateStyle
                paintStyle
                let radius = norm centerOffset
                    centerOffset = Vec2 i j
                    center@(Vec2 centerX centerY) = start +. centerOffset
                    end = Vec2 x y
                    startAngle = angleOfLine (Line center start)
                    endAngle = angleOfLine (Line center end)
                D.moveToVec start
                C.arc centerX centerY radius (getRad startAngle) (getRad endAngle)
                C.stroke
            _otherwise -> pure ()

recordDrawingDistance :: GCode -> Plot ()
recordDrawingDistance instruction = do
    penState <- gets _penState
    penXY@(Vec2 x0 y0) <- gets _penXY
    let distanceTravelled = case instruction of
            G00_LinearRapidMove x y _      -> Just (norm (penXY -. Vec2 (fromMaybe x0 x) (fromMaybe y0 y)))
            G01_LinearFeedrateMove _ x y _ -> Just (norm (penXY -. Vec2 (fromMaybe x0 x) (fromMaybe y0 y)))
            G02_ArcClockwise _ i j x y -> do
                let r = norm (Vec2 i j)
                    center = penXY +. Vec2 i j
                    angle = angleBetween (Line center (Vec2 x y)) (Line center penXY)
                Just (r * getRad (normalizeAngle (deg 0) angle))
            G03_ArcCounterClockwise _ i j x y -> do
                let r = norm (Vec2 i j)
                    center = penXY +. Vec2 i j
                    angle = angleBetween (Line center penXY) (Line center (Vec2 x y))
                Just (r * getRad (normalizeAngle (deg 0) angle))
            _otherwise -> Nothing

    case distanceTravelled of
        Nothing -> pure ()
        Just d -> case penState of
            PenDown -> addDrawingDistance d
            PenUp | isInfinite d -> pure () -- Pen starts at (∞,∞) so we hack around recording it here
            PenUp -> addTravelDistance d

recordBB :: HasBoundingBox object => object -> Plot ()
recordBB object = modify' (\s -> s { _drawnBoundingBox = _drawnBoundingBox s <> boundingBox object })

recordBoundingBox :: GCode -> Plot ()
recordBoundingBox instruction = do
    current@(Vec2 xCurrent yCurrent) <- gets _penXY
    case instruction of
        G00_LinearRapidMove x y _      -> recordBB (Vec2 (fromMaybe xCurrent x) (fromMaybe yCurrent y))
        G01_LinearFeedrateMove _ x y _ -> recordBB (Vec2 (fromMaybe xCurrent x) (fromMaybe yCurrent y))
        G02_ArcClockwise _ i j x y        -> recordBB (Arc        Clockwise current (current +. Vec2 i j) (Vec2 x y))
        G03_ArcCounterClockwise _ i j x y -> recordBB (Arc CounterClockwise current (current +. Vec2 i j) (Vec2 x y))
        _otherwise -> pure ()

addDrawingDistance :: Double -> Plot ()
addDrawingDistance d = modify' (\s -> s { _drawingDistance = _drawingDistance s + d })

addTravelDistance :: Double -> Plot ()
addTravelDistance d = Plot (tell mempty{_penTravelDistance = d})

-- | CwArc a c b = Clockwise arc from a to b with center at c.
data Arc = Arc ArcDirection Vec2 Vec2 Vec2 deriving (Eq, Ord, Show)

data ArcDirection = Clockwise | CounterClockwise deriving (Eq, Ord, Show)

instance HasBoundingBox Arc where
    boundingBox arc@(Arc _ start _ end) =
        boundingBox (start, end, quadrantTransitionBB arc)

quadrantTransitionBB :: Arc -> BoundingBox
quadrantTransitionBB (Arc arcDirection start center end) = case arcDirection of
    Clockwise        -> boundingBox (     go startQuadrant endQuadrant)
    CounterClockwise -> boundingBox (flip go startQuadrant endQuadrant)
  where
    radius = norm (start -. center)
    startQuadrant = whichQuadrant center start
    endQuadrant = whichQuadrant center end

    rightP  = center +. Vec2 radius 0
    leftP   = center -. Vec2 radius 0
    topP    = center +. Vec2 0 radius
    bottomP = center -. Vec2 0 radius
    allP = [bottomP, leftP, topP, rightP]

    arcIsWrapping =
        startQuadrant == endQuadrant
        && case arcDirection of
            Clockwise        -> cross (vectorOf (Line center start)) (vectorOf (Line center end)) > 0
            CounterClockwise -> cross (vectorOf (Line center start)) (vectorOf (Line center end)) < 0

    go QuadrantBR QuadrantBR | arcIsWrapping = allP
    go QuadrantBR QuadrantBR = []
    go QuadrantBR QuadrantBL = [bottomP]
    go QuadrantBR QuadrantTL = [bottomP, leftP]
    go QuadrantBR QuadrantTR = [bottomP, leftP, topP]

    go QuadrantBL QuadrantBR = [leftP, topP, rightP]
    go QuadrantBL QuadrantBL | arcIsWrapping = allP
    go QuadrantBL QuadrantBL = []
    go QuadrantBL QuadrantTL = [leftP]
    go QuadrantBL QuadrantTR = [leftP, topP]

    go QuadrantTL QuadrantBR = [topP, rightP]
    go QuadrantTL QuadrantBL = [topP, rightP, bottomP]
    go QuadrantTL QuadrantTL | arcIsWrapping = allP
    go QuadrantTL QuadrantTL = []
    go QuadrantTL QuadrantTR = [topP]

    go QuadrantTR QuadrantBR = [rightP]
    go QuadrantTR QuadrantBL = [rightP, bottomP]
    go QuadrantTR QuadrantTL = [rightP, bottomP, leftP]
    go QuadrantTR QuadrantTR | arcIsWrapping = allP
    go QuadrantTR QuadrantTR = []

data Quadrant = QuadrantBR | QuadrantBL | QuadrantTL | QuadrantTR deriving (Eq, Ord, Show)

-- | Quadrants are in math coordinates (y pointing upwards!)
whichQuadrant
    :: Vec2 -- ^ Center
    -> Vec2 -- ^ Which quadrant is this point in?
    -> Quadrant
whichQuadrant center point
    | dx >= 0 && dy >= 0 = QuadrantTR
    | dx <  0 && dy >= 0 = QuadrantTL
    | dx <  0 && dy <  0 = QuadrantBL
    | otherwise          = QuadrantBR
  where
    Vec2 dx dy = point -. center

-- | Trace the plotting area to preview the extents of the plot, and wait for
-- confirmation. Useful at the start of a plot.
previewCanvas :: Plot ()
previewCanvas = commented "Preview bounding box" $ do
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
        gCode [ G01_LinearFeedrateMove (Just feedrate) (Just x) (Just y) Nothing ]

-- | Arc interpolation, clockwise
clockwiseArcAroundTo
    :: Vec2 -- ^ Center location
    -> Vec2 -- ^ End position
    -> Plot ()
clockwiseArcAroundTo center (Vec2 x y) = do
    start <- gets _penXY
    let Vec2 centerXRel centerYRel = vectorOf (Line start center)
    feedrate <- asks _feedrate
    penDown
    gCode [ G02_ArcClockwise (Just feedrate) centerXRel centerYRel x y ]

-- | Arc interpolation, counterclockwise
counterclockwiseArcAroundTo
    :: Vec2 -- ^ Center location
    -> Vec2 -- ^ End position
    -> Plot ()
counterclockwiseArcAroundTo center (Vec2 x y) = do
    start <- gets _penXY
    let Vec2 centerXRel centerYRel = vectorOf (Line start center)
    feedrate <- asks _feedrate
    penDown
    gCode [ G03_ArcCounterClockwise (Just feedrate) centerXRel centerYRel x y ]

-- | If the pen is up, lower it to drawing height. Do nothing if it is already
-- lowered.
penDown :: Plot ()
penDown = gets _penState >>= \case
    PenDown -> pure ()
    PenUp -> do
        zDrawing <- asks _zDrawingHeight
        zFeedrate <- asks _zLoweringFeedrate
        case zFeedrate of
            Nothing -> gCode [ G00_LinearRapidMove Nothing Nothing (Just zDrawing) ]
            Just fr -> gCode [ G01_LinearFeedrateMove (Just fr) Nothing Nothing (Just zDrawing) ]
        Plot (tell mempty{_elementsDrawn = 1})
        modify' (\s -> s { _penState = PenDown })

-- | If the pen is down, lift it to travel height. Do nothing if it is already
-- lifted.
penUp :: Plot ()
penUp = gets _penState >>= \case
    PenUp -> pure ()
    PenDown -> do
        zTravel <- asks _zTravelHeight
        gCode [ G00_LinearRapidMove Nothing Nothing (Just zTravel) ]
        modify' (\s -> s { _penState = PenUp })

-- | Locally change the feedrate
withFeedrate :: Double -> Plot a -> Plot a
withFeedrate f = local (\settings -> settings { _feedrate = f })

-- | Locally adapt the z drawing height (e.g. for changing pen pressure)
withDrawingHeight :: Double -> Plot a -> Plot a
withDrawingHeight z = local (\settings -> settings { _zDrawingHeight = z })

-- | Group the commands generated by the arguments in a block. This is purely
-- cosmetical for the generated GCode.
block :: Plot a -> Plot a
block (Plot content) = Plot (mapRWS (\(a, s, writerLog) -> (a, s, writerLog{_plottedGCode = DL.singleton (GBlock (DL.toList (_plottedGCode writerLog)))})) content)

-- | Add a GCode comment.
comment :: Text -> Plot ()
comment txt = gCode [ GComment txt ]

-- | Having a block with a comment ontop of it is a common pattern, so here’s a helper for that.
commented :: Text -> Plot a -> Plot a
commented caption content = do
    comment caption
    block content

-- | Pause the plot for later resumption at the current state.
pause :: PauseMode -> Plot ()
pause PauseUserConfirm = gCode [ M0_Pause ]
pause (PauseSeconds seconds) = gCode [ G04_Dwell_ms (seconds*1000) ]

data PauseMode
    = PauseUserConfirm -- ^ Wait until user confirmation, e.g. in a web UI or with a button. (M0/Pause)
    | PauseSeconds Double -- ^ Wait for a certain time (G4/Dwell)
    deriving (Eq, Ord, Show)

-- | Distance drawn so far.
--
-- One use case is adding a pause when a pencil needs sharpening again.
drawingDistance :: Plot Double
drawingDistance = gets _drawingDistance

addHeaderFooter :: PlottingSettings -> PlottingWriterLog -> PlottingState -> DList GCode
addHeaderFooter settings writerLog finalState = mconcat [header, body, footer]
  where
    body = _plottedGCode writerLog

    boundingBoxCheck = case (_previewDrawnShapesBoundingBox settings, _drawnBoundingBox finalState) of
        (False, _) -> GBlock []
        (True, BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax)) -> GBlock
            [ GComment "Trace bounding box"
            , GComment (format ("x = [" % fixed 3 % ".." % fixed 3 % "]") xMin xMax)
            , GComment (format ("y = [" % fixed 3 % ".." % fixed 3 % "]") yMin yMax)
            , GBlock
                [ G00_LinearRapidMove (Just xMin) (Just yMin) Nothing
                , G00_LinearRapidMove Nothing Nothing (Just (_zTravelHeight settings))
                , G93_Feedrate_TravelInFractionOfMinute
                , G04_Dwell_ms 0.5
                -- 60/n ==> n seconds to move
                , G01_LinearFeedrateMove (Just (60/3)) (Just xMax) (Just yMin) Nothing
                , G04_Dwell_ms 0.5
                , G01_LinearFeedrateMove (Just (60/3)) (Just xMax) (Just yMax) Nothing
                , G04_Dwell_ms 0.5
                , G01_LinearFeedrateMove (Just (60/3)) (Just xMin) (Just yMax) Nothing
                , G04_Dwell_ms 0.5
                , G01_LinearFeedrateMove (Just (60/3)) (Just xMin) (Just yMin) Nothing
                , G94_Feedrate_UnitsPerMinute
                , M0_Pause
                ]
            ]

    setDefaultModes = GBlock
        [ G17_Plane_XY
        , G21_UseMm
        , G90_AbsoluteMovement
        , G94_Feedrate_UnitsPerMinute
        ]

    header = DL.fromList
        [ GComment "Header"
        , GBlock
            [ GComment "Normalize modal settings"
            , setDefaultModes ]
        , boundingBoxCheck
        , GBlock
            [ GComment (format ("Total drawing distance: " % fixed 1 % "m") (_drawingDistance finalState /1000))
            , GComment (format ("Total travel (non-drawing) distance: " % fixed 1 % "m") (_penTravelDistance writerLog/1000))
            , GComment (format ("Total number of elements (pen down events): " % int) (_elementsDrawn writerLog))
            ]
        ]

    footer = DL.fromList
        [ GComment "Footer"
        , finishMoveCheck
        ]

    finishMoveCheck = GBlock $ case _finishMove settings of
        Nothing ->
            [ GComment "Lift pen"
            , GBlock [G00_LinearRapidMove Nothing Nothing (Just 10)]
            ]
        Just FinishWithG28 ->
            [ GComment "Move to predefined position"
            , GBlock
                [ G00_LinearRapidMove Nothing Nothing (Just (_zTravelHeight settings))
                , G28_GotoPredefinedPosition Nothing Nothing Nothing
                ]
            ]
        Just FinishWithG30 ->
            [ GComment "Move to predefined position"
            , GBlock
                [ G00_LinearRapidMove Nothing Nothing (Just (_zTravelHeight settings))
                , G30_GotoPredefinedPosition Nothing Nothing Nothing
                ]
            ]

decorateCairoPreview :: PlottingSettings -> PlottingState -> C.Render ()
decorateCairoPreview settings finalState = D.cairoScope $ when (_previewDecorate settings) $ do
    let drawnBB = if _previewDrawnShapesBoundingBox settings
            then Just (_drawnBoundingBox finalState)
            else Nothing
        zeroMarker = do
            D.sketch (Line (Vec2 (-10) 0) (Vec2 10 0))
            D.sketch (Line (Vec2 0 (-10)) (Vec2 0 10))
            C.stroke
    D.setColor (D.mathematica97 2)
    zeroMarker
    for_ [drawnBB, _canvasBoundingBox settings] $ \case
        Nothing -> pure ()
        Just bb -> do
            D.sketch (boundingBoxPolygon bb)
            C.stroke

-- | Result of 'runPlot'; unifies convenience API and internals for tinkering.
data RunPlotResult = RunPlotResult
    { _plotGCode :: [GCode]
        -- ^ The generated G code. Use 'writeGCodeFile' to store it to a file.

    , _plotPreview :: C.Render ()
        -- ^ Preview for the generated GCode. Use 'renderPreview' to convert it
        -- to an SVG or PNG file, or 'D.render' for more control in rendering.

    , _plotBoundingBox :: BoundingBox
        -- ^ The 'BoundingBox' of the resulting plot.

    , _totalBoundingBox :: BoundingBox
        -- ^ The total 'BoundingBox' of the preview, including origin and canvas.
        --
        -- Example to show the entire bounding box in the preview:
        --
        -- @
        -- let bb = '_plotBoundingBox' result
        --     (w, h) = 'boundingBoxSize' bb
        --     trafo = 'transformBoundingBox' bb ('BoundingBox' 'zero' ('Vec2' w h)) 'def'
        -- 'D.render' previewFileName w h $ do
        --     'C.transform ('D.toCairoMatrix' trafo)
        --     '_plotPreview' result
        -- @

    , _plotInternals :: TinkeringInternals
        -- ^ Internals calculated along the way. Useful for tinkering and testing.
    }

data TinkeringInternals = TinkeringInternals
    { _tinkeringSettings :: PlottingSettings -- ^ The settings used to run the plot.
    , _tinkeringWriterLog :: PlottingWriterLog
        -- ^ Writer log, decorated with information only available after the plot
        -- finishes. The GCode has header and footer, the Cairo preview includes
        -- bounding boxes, etc.
    , _tinkeringState :: PlottingState
        -- ^ Final state after running the plot. Includes data such as the
        -- total pen travel distance.
    }

writeGCodeFile :: FilePath -> RunPlotResult -> IO ()
writeGCodeFile file = TL.writeFile file . renderGCode . _plotGCode

renderPreview :: FilePath -> RunPlotResult -> IO ()
renderPreview file result = do
    let bb = _totalBoundingBox result
        (w, h) = boundingBoxSize bb
        trafo = transformBoundingBox bb (boundingBox [zero, Vec2 w h]) def
    D.render file (round w) (round h) $ do
        D.coordinateSystem (D.MathStandard_ZeroBottomLeft_XRight_YUp h)
        C.transform (D.toCairoMatrix trafo)
        _plotPreview result

-- | Run the 'Plot' to easily generate the resulting GCode file. For convenience, this also generates a Cairo-based preview of the geometry.
--
-- @
-- let plotResult = 'runPlot' settings body
-- '_writeGCodeFile' plotResult "output.g"
-- '_writePreviewFile' plotResult "output.png"
-- @
runPlot
    :: PlottingSettings
    -> Plot a
    -> RunPlotResult
runPlot settings body =
    let (_, finalState, writerLog) = runRWS body' settings initialState
        Plot body' = body
        initialState = PlottingState
            { _penState = PenUp
            , _penXY = Vec2 (1/0) (1/0) -- Nonsense value so we’re always misaligned in the beginning, making every move command actually move
            , _drawingDistance = 0
            , _drawnBoundingBox = mempty
            }

        decoratedGCode = addHeaderFooter settings writerLog finalState

        decoratedCairoPreview = D.cairoScope (decorateCairoPreview settings finalState >> _plottingCairoPreview writerLog)
        canvasBB = _canvasBoundingBox settings
        totalBB = mconcat
            [ _drawnBoundingBox finalState
            , boundingBox canvasBB
            , boundingBox (zero :: Vec2)
            ]

        decoratedWriterLog = writerLog{_plottedGCode=decoratedGCode, _plottingCairoPreview=decoratedCairoPreview}
    in RunPlotResult
        { _plotGCode = DL.toList decoratedGCode
        , _plotPreview = decoratedCairoPreview
        , _plotBoundingBox = _drawnBoundingBox finalState
        , _totalBoundingBox = totalBB
        , _plotInternals = TinkeringInternals
            { _tinkeringSettings = settings
            , _tinkeringWriterLog = decoratedWriterLog
            , _tinkeringState = finalState
            }
        }

-- | Draw a shape by lowering the pen, setting the right speed, etc. The specifics
-- are defined in the configuration given in 'runPlot', or by the various utility
-- functions such as 'withFeedrate' or 'withDrawingHeight'
class Plotting a where
    plot :: a -> Plot ()

-- | Trace the bounding box without actually drawing anything to estimate result size
instance Plotting BoundingBox where
    plot (BoundingBox start@(Vec2 xMin yMin) (Vec2 xMax yMax)) = commented "Hover over bounding box" $ do
        repositionTo start
        gCode
            [ G93_Feedrate_TravelInFractionOfMinute
            , G04_Dwell_ms 500
            -- 60/n ==> n seconds to move
            , G01_LinearFeedrateMove (Just (60/3)) (Just xMax) (Just yMin) Nothing
            , G04_Dwell_ms 500
            , G01_LinearFeedrateMove (Just (60/3)) (Just xMax) (Just yMax) Nothing
            , G04_Dwell_ms 500
            , G01_LinearFeedrateMove (Just (60/3)) (Just xMin) (Just yMax) Nothing
            , G04_Dwell_ms 500
            , G01_LinearFeedrateMove (Just (60/3)) (Just xMin) (Just yMin) Nothing
            , G94_Feedrate_UnitsPerMinute
            ]

instance Plotting Line where
    plot (Line a b) = commented "Line" $ do
        current <- gets _penXY
        let (start, end) = if norm (current -. a) <= norm (current -. b)
                then (a,b)
                else (b,a)
        repositionTo start
        lineTo end

instance Plotting Circle where
    plot (Circle center radius) = commented "Circle" $ do
        -- The naive way of painting a circle is by always starting them e.g.
        -- on the very left. This requires some unnecessary pen hovering, and
        -- for some pens creates a visible »pen down« dot. We therefore go the
        -- more complicated route here: start the circle at the point closest
        -- to the pen position. We only fall back to the naive way if the
        -- circles are very small.
        current <- gets _penXY
        let distanceCenterCurrent = norm (center -. current) -- Might be infinite if the current point isn’t defined yet!
            radial = if 0.1 <= distanceCenterCurrent && not (isInfinite distanceCenterCurrent)
                then radius *. direction (Line center current)
                else Vec2 radius 0
            start = center +. radial
            opposite = center -. radial

        -- FluidNC 3.4.2 has a bug where small circles (2mm radius) sometimes don’t
        -- do anything when we plot it with a single arc »from start to itself«. We
        -- work around this by explicitly chaining two half circles.
        repositionTo start
        clockwiseArcAroundTo center opposite
        clockwiseArcAroundTo center start

-- | Approximation by a number of points
instance Plotting Ellipse where
    plot (Ellipse trafo) = commented "Ellipse" $ do
        plot (transform trafo (regularPolygon 64))

instance Foldable f => Plotting (Polyline f) where
    plot (Polyline xs) = go (toList xs)
      where
        go [] = pure ()
        go points = commented "Polyline" $ do
            current <- gets _penXY
            let p:ointsToPlot = if norm (current -. head points) < norm (current -. last points)
                    then points
                    else reverse points
            repositionTo p
            traverse_ lineTo ointsToPlot

-- | Draw each element (in order)
instance (Functor f, Sequential f, Plotting a) => Plotting (f a) where
    plot x = commented "Sequential" (traverse_ plot x)

-- | Draw each element (in order)
instance (Plotting a, Plotting b) => Plotting (a,b) where
    plot (a,b) = commented "2-tuple" (plot a >> plot b)

-- | Draw each element (in order)
instance (Plotting a, Plotting b, Plotting c) => Plotting (a,b,c) where
    plot (a,b,c) = commented "3-tuple" (plot a >> plot b >> plot c)

-- | Draw each element (in order)
instance (Plotting a, Plotting b, Plotting c, Plotting d) => Plotting (a,b,c,d) where
    plot (a,b,c,d) = commented "4-tuple" (plot a >> plot b >> plot c >> plot d)

-- | Draw each element (in order)
instance (Plotting a, Plotting b, Plotting c, Plotting d, Plotting e) => Plotting (a,b,c,d,e) where
    plot (a,b,c,d,e) = commented "5-tuple" (plot a >> plot b >> plot c >> plot d >> plot e)

instance Plotting Polygon where
    -- Like polyline, but closes up the shape
    plot (Polygon []) = pure ()
    plot (Polygon corners) = commented "Polygon" $ do
        current <- gets _penXY
        let Just closestCorner = minimumOn (\corner -> norm (current -. corner)) corners
            (before, after) = break (== closestCorner) corners
            r:eorderedCorners = after ++ before

        repositionTo r
        traverse_ lineTo eorderedCorners
        lineTo r

-- | FluidNC doesn’t support G05, so we approximate Bezier curves with line pieces.
-- We use the naive Bezier interpolation 'bezierSubdivideT', because it just so
-- happens to put more points in places with more curvature.
instance Plotting Bezier where
    plot bezier = commented "Bezier (cubic)" $ do
        let points = bezierSubdivideT 32 bezier
        let p:ointsToPlot = points
        repositionTo p
        traverse_ lineTo ointsToPlot

minimumOn :: (Foldable f, Ord ord) => (a -> ord) -> f a -> Maybe a
minimumOn f xs
    | null xs = Nothing
    | otherwise = Just (minimumBy (\x y -> compare (f x) (f y)) xs)

data MinimizePenHoveringSettings a = MinimizePenHoveringSettings
    { _getStartEndPoint :: a -> (Vec2, Vec2)
    , _flipObject :: Maybe (a -> a)
    , _mergeObjects :: Maybe (a -> a -> Maybe a)
    }

-- | Similar to 'minimizePenHovering', but for arbitrary objects with a given start and end point.
minimizePenHoveringBy :: Ord a => MinimizePenHoveringSettings a -> S.Set a -> [a]
minimizePenHoveringBy settings = sortStep zero . mergeStep
  where
    distanceNorm = case _flipObject settings of
        Nothing -> \penPos object -> let (a, _) = _getStartEndPoint settings object in norm (a -. penPos)
        Just _  -> \penPos object -> let (a, b) = _getStartEndPoint settings object in min (norm (a -. penPos)) (norm (b -. penPos))
    reverseDistanceNorm = case _flipObject settings of
        Nothing -> \penPos object -> let (_, b) = _getStartEndPoint settings object in norm (b -. penPos)
        Just _  -> \penPos object -> let (a, b) = _getStartEndPoint settings object in min (norm (a -. penPos)) (norm (b -. penPos))
    rightWayRound = case _flipObject settings of
        Nothing -> \_ object ->
            let (_, b) = _getStartEndPoint settings object
            in  (object, b)
        Just flipObject -> \penPos object ->
            let (a, b) = _getStartEndPoint settings object
            in  if norm (a -. penPos) > norm (b -. penPos)
                then (flipObject object, a)
                else (object, b)
    reverseRightWayRound = case _flipObject settings of
        Nothing -> \_ object -> object
        Just flipObject -> \penPos object ->
            let (a, b) = _getStartEndPoint settings object
            in  if norm (a -. penPos) < norm (b -. penPos)
                then flipObject object
                else object
    -- Sort by minimal travel between adjacent lines
    sortStep penPos pool =
        let closestNextObject = minimumOn (distanceNorm penPos) pool
        in case closestNextObject of
            Nothing -> []
            Just object ->
                let (object', end) = rightWayRound penPos object
                    remainingPool = S.delete object pool
                    newPenPos = end
                in object' : sortStep newPenPos remainingPool
    mergeStep = case _mergeObjects settings of
        Nothing -> id
        Just merge -> \pool -> go pool S.empty
          where
            go pool result =
                let closestNextObject = minimumOn (distanceNorm zero) pool
                in case closestNextObject of
                    Nothing -> result
                    Just object ->
                        let result' = tryMerge object result
                        in  go (S.delete object pool) result'
            tryMerge object pool =
                let (a, b) = _getStartEndPoint settings object
                    closestNextObject = minimumOn (distanceNorm b) pool
                    closestPreviousObject = minimumOn (reverseDistanceNorm a) pool
                in case (closestPreviousObject, closestNextObject) of
                    (Just prevObject, Just nextObject) -- missing link between two objects
                        | Just object' <- merge (reverseRightWayRound a prevObject) object, Just object'' <- merge object' (fst $ rightWayRound b nextObject)
                        -> S.insert object'' . S.delete prevObject . S.delete nextObject $ pool
                    (Just prevObject, _) -- object can be appended
                        | Just object' <- merge (reverseRightWayRound a prevObject) object
                        -> S.insert object' . S.delete prevObject $ pool
                    (_, Just nextObject) -- object can be prepended
                        | Just object' <- merge object (fst $ rightWayRound b nextObject)
                        -> S.insert object' . S.delete nextObject $ pool
                    _otherwise
                        -> S.insert object pool

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
