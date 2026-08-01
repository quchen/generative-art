-- | Visual debug helpers for pure geometry code.
--
-- Pure geometry algorithms such as the Margalit–Knott polygon clipper are
-- easiest to diagnose when the inputs and outputs can be seen. This module
-- provides 'traceSketch' and friends: they render the input and output of a
-- pure function to an SVG file and return the function result unchanged. The
-- side effect (writing a file and printing a line) is performed via
-- 'unsafePerformIO', so the helpers can be inserted into pure code without
-- changing its type.
module Draw.Trace
    ( traceSketch
    , traceSketchWith
    , traceSketchWithBB
    ) where

import           Data.IORef
import           System.IO.Unsafe              (unsafePerformIO)
import           Text.Printf

import           Draw                          (Sketch (sketch), render, coordinateSystem, CoordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp), setColor, cairoScope, withOpacity, mma)
import           Geometry.Core                 (HasBoundingBox (boundingBox), BoundingBox (BoundingBox), Vec2 (Vec2), boundingBoxSize, growBoundingBox)
import           Graphics.Rendering.Cairo      as C hiding (x, y)

{-# NOINLINE traceCounter #-}
traceCounter :: IORef Int
traceCounter = unsafePerformIO (newIORef 0)

-- | @traceSketch label input output@ writes an SVG file showing @input@ and
-- @output@, prints a debug line with the file name and the bounding box, and
-- returns @output@ unchanged.
--
-- Both @input@ and @output@ must have 'Sketch' and 'HasBoundingBox' instances
-- (tuples, lists, 'Polygon', etc. all do). When the output does /not/ have a
-- 'HasBoundingBox' instance (e.g. @[(Polygon, IslandOrHole)]@), use
-- 'traceSketchWith' or 'traceSketchWithBB' instead.
--
-- This function performs 'IO' (file write + 'putStrLn') as a side effect via
-- 'unsafePerformIO'. Use it for debugging only.
traceSketch
    :: (Sketch input, HasBoundingBox input, Show input, Sketch output, HasBoundingBox output, Show output)
    => String      -- ^ Label identifying the call site, e.g. @"differencePP"@.
    -> input       -- ^ The pure function’s input.
    -> output      -- ^ The pure function’s output.
    -> output
traceSketch label input output =
    traceSketchWithBB label bb renderInput renderOutput input output
  where
    bb = boundingBox (input, output)
    renderInput  _ = sketch input  >> setColor (mma 0) >> C.setLineWidth 1.5 >> C.stroke
    renderOutput _ = sketch output >> setColor (mma 1 `withOpacity` 0.4) >> C.fill
                            >> sketch output >> setColor (mma 1) >> C.setLineWidth 1.5 >> C.stroke

-- | Like 'traceSketch', but with explicit rendering actions for input and
-- output. Useful when the generic 'sketch' instances do not produce the
-- desired visualization (e.g. you want to color individual elements of a list
-- differently). The bounding box is computed from @(input, output)@, so both
-- must have a 'HasBoundingBox' instance.
traceSketchWith
    :: (HasBoundingBox input, Show input, HasBoundingBox output, Show output)
    => String        -- ^ Label.
    -> (input -> C.Render ())   -- ^ How to draw the input.
    -> (output -> C.Render ()) -- ^ How to draw the output.
    -> input
    -> output
    -> output
traceSketchWith label renderInput renderOutput input output =
    traceSketchWithBB label (boundingBox (input, output)) renderInput renderOutput input output

-- | Like 'traceSketchWith', but with an explicit 'BoundingBox'. Use this when
-- the output does not have a 'HasBoundingBox' instance (e.g. a list of tagged
-- polygons); pass the bounding box of the input (or any other suitable
-- geometry that covers everything that will be drawn).
traceSketchWithBB
    :: (Show input, Show output)
    => String        -- ^ Label.
    -> BoundingBox   -- ^ Bounding box covering everything that will be drawn.
    -> (input -> C.Render ())   -- ^ How to draw the input.
    -> (output -> C.Render ()) -- ^ How to draw the output.
    -> input
    -> output
    -> output
traceSketchWithBB label userBB renderInput renderOutput input output = unsafePerformIO $ do
    n <- atomicModifyIORef' traceCounter (\i -> (i + 1, i))
    let fileName = printf "debug/trace_%s_%04d.svg" (sanitize label) n
        bb = growBoundingBox margin userBB
        BoundingBox (Vec2 xLo yLo) _ = bb
        (width, height) = boundingBoxSize bb
        -- Always give the canvas a little headroom so strokes are not clipped
        -- at the bounding-box edge, and enforce a minimum size for degenerate
        -- (e.g. single-point) inputs.
        margin = 10
        pxW = max 100 (ceiling (width  + 2 * margin))
        pxH = max 100 (ceiling (height + 2 * margin))
    render fileName pxW pxH $ do
        coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp (fromIntegral pxH))
        -- Translate so the bounding box’s bottom-left sits at (margin, margin)
        -- in canvas coordinates (canvas origin is top-left; after the y-flip
        -- of the math coordinate system, +y goes up, so we translate by the
        -- negative of the lower corner).
        C.translate (margin - xLo) (margin - yLo)
        cairoScope (renderInput input)
        cairoScope (renderOutput output)
    putStrLn $ printf "[traceSketch %s] %s\n  input:     %s\n  output: %s"
        label fileName (show input) (show output)
    pure output
  where
    sanitize = map (\c -> if c == '/' || c == ' ' then '_' else c)
