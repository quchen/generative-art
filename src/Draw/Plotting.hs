{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw.Plotting (
      runPlot
    , PlottingSettings(..)
    , draw
    , Plot()
    , Plotting(..)
    , GCode(..)
    , withHeaderFooter

    -- * Utilities
    , minimizePenHovering
) where



import Control.Monad.State
import Control.Monad.Writer
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
    { _previewBoundingBox :: Maybe BoundingBox -- ^ Trace this bounding box to preview extents of the plot and wait for confirmation
    , _feedrate :: Maybe Double -- ^ Either set a feedrate, or have an initial check whether one was set previously
    } deriving (Eq, Ord, Show)

draw :: Plot a -> Plot a
draw content = gBlock $ do
    tell [ G00_LinearRapidMove Nothing Nothing (Just (-1)) ]
    a <- content
    tell [ G00_LinearRapidMove Nothing Nothing (Just 1) ]
    pure a

gBlock :: Plot a -> Plot a
gBlock (Plot content) = Plot $ mapStateT (mapWriter (\(a, g) -> (a, [GBlock g]))) content

withHeaderFooter :: Plot a -> Plot a
withHeaderFooter body = gBlock $ do
    header
    a <- body
    footer
    pure a
  where
    feedrateCheck = gets _feedrate >>= \case
        Just f -> tell
            [ GComment "Initial feedrate"
            , F_Feedrate f
            ]
        Nothing -> tell
            [ GComment "NOOP move to make sure feedrate is already set externally"
            , G91_RelativeMovement
            , G01_LinearFeedrateMove (Just 0) (Just 0) (Just 0)
            , G90_AbsoluteMovement
            ]

    previewBoundingBox = gets _previewBoundingBox >>= \case
        Just bb -> do
            plot $ GComment "Preview bounding box"
            plot bb
            plot M0_Pause
        Nothing -> pure ()

    header = gBlock $ do
        tell [ GComment "Header" ]
        feedrateCheck
        previewBoundingBox

    footer = gBlock $ tell
            [ GComment "Footer"
            , GComment "Lift pen"
            , G00_LinearRapidMove Nothing Nothing (Just 10)
            ]

runPlot :: PlottingSettings -> Plot a -> TL.Text
runPlot settings (Plot body) = renderGCode (execWriter (evalStateT body settings))

class Plotting a where
    plot :: a -> Plot ()

instance Plotting GCode where
    plot = tell . pure

-- | Trace the bounding box without actually drawing anything to estimate result size
instance Plotting BoundingBox where
    plot (BoundingBox (Vec2 xMin yMin) (Vec2 xMax yMax)) = gBlock $ do
        plot $ GComment "Hover over bounding box"
        plot $ G00_LinearRapidMove (Just xMin) (Just yMin) Nothing
        plot $ G00_LinearRapidMove (Just xMax) (Just yMin) Nothing
        plot $ G00_LinearRapidMove (Just xMax) (Just yMax) Nothing
        plot $ G00_LinearRapidMove (Just xMin) (Just yMax) Nothing
        plot $ G00_LinearRapidMove (Just xMin) (Just yMin) Nothing

instance Plotting Line where
    plot (Line (Vec2 a b) (Vec2 x y)) = gBlock $ do
        plot $ GComment "Line"
        plot $ G00_LinearRapidMove (Just a) (Just b) Nothing
        draw $ plot (G01_LinearFeedrateMove (Just x) (Just y) Nothing)

instance Plotting Circle where
    plot (Circle (Vec2 x y) r) = gBlock $ do
        let (startX, startY) = (x-r, y)
        plot $ GComment "Circle"
        plot $ G00_LinearRapidMove (Just startX) (Just startY) Nothing
        draw $ plot (G02_ArcClockwise r 0 startX startY)

-- | Approximation by a number of points
instance Plotting Ellipse where
    plot (Ellipse trafo) = gBlock $ do
        plot $ GComment "Ellipse"
        plot $ transform trafo (regularPolygon 64)

-- | Polyline
instance {-# OVERLAPPING #-} Sequential f => Plotting (f Vec2) where
    plot = go . toList
      where
        go [] = pure ()
        go (Vec2 startX startY : points) = gBlock $ do
            plot $ GComment "Polyline"
            plot $ G00_LinearRapidMove (Just startX) (Just startY) Nothing
            draw $ plot (GBlock [ G01_LinearFeedrateMove (Just x) (Just y) Nothing | Vec2 x y <- points])

-- | Draw each element separately. Note the overlap with the Polyline instance, which takes precedence.
instance {-# OVERLAPPABLE #-} (Functor f, Sequential f, Plotting a) => Plotting (f a) where
    plot x = gBlock $ do
        plot (GComment "Sequential")
        traverse_ plot x

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (Plotting a, Plotting b) => Plotting (a,b) where
    plot (a,b) = gBlock $ do
        plot (GComment "2-tuple")
        plot a
        plot b

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (Plotting a, Plotting b, Plotting c) => Plotting (a,b,c) where
    plot (a,b,c) = gBlock $ do
        plot (GComment "3-tuple")
        plot a
        plot b
        plot c

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (Plotting a, Plotting b, Plotting c, Plotting d) => Plotting (a,b,c,d) where
    plot (a,b,c,d) = gBlock $ do
        plot (GComment "4-tuple")
        plot a
        plot b
        plot c
        plot d

-- | Draw each element (in order)
instance {-# OVERLAPPING #-} (Plotting a, Plotting b, Plotting c, Plotting d, Plotting e) => Plotting (a,b,c,d,e) where
    plot (a,b,c,d,e) = gBlock $ do
        plot (GComment "5-tuple")
        plot a
        plot b
        plot c
        plot d
        plot e

instance Plotting Polygon where
    plot (Polygon []) = pure ()
    plot (Polygon (p:ps)) = gBlock $ do -- Like polyline, but closes up the shape
        let Vec2 startX startY = p
        plot $ GComment "Polygon"
        plot $ G00_LinearRapidMove (Just startX) (Just startY) Nothing
        draw $ plot (GBlock [G01_LinearFeedrateMove (Just x) (Just y) Nothing | Vec2 x y <- ps ++ [p]])

-- | FluidNC doesn’t support G05, so we approximate Bezier curves with line pieces.
-- We use the naive Bezier interpolation 'bezierSubdivideT', because it just so
-- happens to put more points in places with more curvature.
instance Plotting Bezier where
    plot bezier@(Bezier a _ _ _) = gBlock $ do
        let Vec2 startX startY = a
        plot $ GComment "Bezier (cubic)"
        plot $ G00_LinearRapidMove (Just startX) (Just startY) Nothing
        draw $ plot (GBlock [G01_LinearFeedrateMove (Just x) (Just y) Nothing | Vec2 x y <- bezierSubdivideT 32 bezier])

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
