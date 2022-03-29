{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw.Plotting (
    -- * 'Plot' Monad
      Plot()
    , PlottingSettings(..)
    , runPlot
    , withHeaderFooter

    -- * 'Plotting' shapes
    , Plotting(..)

    -- * Plotting primitives
    , moveTo
    , lineTo
    , clockwiseArcAroundTo
    , counterclockwiseArcAroundTo
    , setFeedrate
    , pause

    -- ** File structure
    , block
    , comment

    -- * Raw G-Code
    , draw
    , gCode

    -- * Utilities
    , minimizePenHovering
) where



import Control.Monad.State
import Control.Monad.Writer
import Data.Default.Class
import           Data.Foldable
import qualified Data.Set       as S
import qualified Data.Text.Lazy as TL
import           Data.Vector    (Vector)
import qualified Data.Vector    as V

import Draw.Plotting.GCode
import Geometry.Bezier
import Geometry.Core
import Geometry.Shapes


newtype Plot a = Plot (StateT PlottingSettings (Writer [GCode]) a)
    deriving (Functor, Applicative, Monad, MonadWriter [GCode], MonadState PlottingSettings)

data PlottingSettings = PlottingSettings
    { _previewBoundingBox :: Maybe BoundingBox
    -- ^ Trace this bounding box to preview extents of the plot and wait for confirmation

    , _feedrate :: Maybe Double
    -- ^ Either set a feedrate, or have an initial check whether one was set previously

    , _zTravelHeight :: Double
    -- ^ During travel motion, keep the pen at this height (in absolute coordinates)

    , _zDrawingHeight :: Double
    -- ^ When drawing, keep the pen at this height (in absolute coordinates)
    } deriving (Eq, Ord, Show)

instance Default PlottingSettings where
    def = PlottingSettings
        { _previewBoundingBox = Nothing
        , _feedrate = Nothing
        , _zTravelHeight = 1
        , _zDrawingHeight = -1
        }

gCode :: [GCode] -> Plot ()
gCode = tell

moveTo :: Vec2 -> Plot ()
moveTo (Vec2 x y) = gCode [ G00_LinearRapidMove (Just x) (Just y) Nothing ]

lineTo :: Vec2 -> Plot ()
lineTo (Vec2 x y) = draw $ gCode [ G01_LinearFeedrateMove (Just x) (Just y) Nothing ]

-- | Center given in _relative_ coordinates, but target in _absolute_ coordinates!
counterclockwiseArcAroundTo :: Vec2 -> Vec2 -> Plot ()
counterclockwiseArcAroundTo (Vec2 mx my) (Vec2 x y) = draw $ gCode [ G03_ArcCounterClockwise mx my x y ]

-- | Center given in _relative_ coordinates, but target in _absolute_ coordinates!
clockwiseArcAroundTo :: Vec2 -> Vec2 -> Plot ()
clockwiseArcAroundTo (Vec2 mx my) (Vec2 x y) = draw $ gCode [ G02_ArcClockwise mx my x y ]

draw :: Plot a -> Plot a
draw content = block $ do
    zDrawing <- gets _zDrawingHeight
    zTravel <- gets _zTravelHeight
    gCode [ G00_LinearRapidMove Nothing Nothing (Just zDrawing) ]
    a <- content
    gCode [ G00_LinearRapidMove Nothing Nothing (Just zTravel) ]
    pure a

setFeedrate :: Double -> Plot ()
setFeedrate f = gCode [ F_Feedrate f ]

block :: Plot a -> Plot a
block (Plot content) = Plot $ mapStateT (mapWriter (\(a, g) -> (a, [GBlock g]))) content

comment :: TL.Text -> Plot ()
comment txt = gCode [ GComment txt ]

pause :: Plot ()
pause = gCode [ M0_Pause ]

withHeaderFooter :: Plot a -> Plot a
withHeaderFooter body = block $ do
    header
    a <- body
    footer
    pure a
  where
    feedrateCheck = gets _feedrate >>= \case
        Just f -> comment "Initial feedrate" >> setFeedrate f
        Nothing -> gCode
            [ GComment "NOOP move to make sure feedrate is already set externally"
            , G91_RelativeMovement
            , G01_LinearFeedrateMove (Just 0) (Just 0) (Just 0)
            , G90_AbsoluteMovement
            ]

    previewBoundingBox = gets _previewBoundingBox >>= \case
        Just bb -> do
            comment "Preview bounding box"
            plot bb
            pause
        Nothing -> pure ()

    header = block $ do
        comment "Header"
        feedrateCheck
        previewBoundingBox

    footer = block $ do
            comment "Footer"
            comment "Lift pen"
            gCode [ G00_LinearRapidMove Nothing Nothing (Just 10) ]

runPlot :: PlottingSettings -> Plot a -> TL.Text
runPlot settings (Plot body) = renderGCode (execWriter (evalStateT body settings))

class Plotting a where
    plot :: a -> Plot ()

-- | Trace the bounding box without actually drawing anything to estimate result size
instance Plotting BoundingBox where
    plot (BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax)) = block $ do
        comment "Hover over bounding box"
        moveTo (Vec2 xMin yMin)
        moveTo (Vec2 xMax yMin)
        moveTo (Vec2 xMax yMax)
        moveTo (Vec2 xMin yMax)
        moveTo (Vec2 xMin yMin)

instance Plotting Line where
    plot (Line start end) = block $ do
        comment "Line"
        moveTo start
        lineTo end

instance Plotting Circle where
    plot (Circle center radius) = block $ do
        let start = center -. Vec2 radius 0
        comment "Circle"
        moveTo start
        clockwiseArcAroundTo (center -. start) start

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
            moveTo p
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
        moveTo p
        traverse_ lineTo ps
        lineTo p

-- | FluidNC doesn’t support G05, so we approximate Bezier curves with line pieces.
-- We use the naive Bezier interpolation 'bezierSubdivideT', because it just so
-- happens to put more points in places with more curvature.
instance Plotting Bezier where
    plot bezier@(Bezier a _ _ _) = block $ do
        comment "Bezier (cubic)"
        moveTo a
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
