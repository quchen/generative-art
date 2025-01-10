module Debug.Trace.Sketch where

import Draw
import Geometry.Core

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.IO (unsafePerformIO)
import Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Matrix as C
import Debug.Trace (traceM)

traceSketch :: (HasBoundingBox a, Sketch a) => String -> a -> b -> b
traceSketch msg a b = unsafePerformIO $ do
    let BoundingBox (Vec2 x0 y0) (Vec2 x1 y1) = boundingBox a
        -- Cairo does not support image widths/heights above this threshold
        w = min 32767 $ ceiling (x1 - x0) + 10
        h = min 32767 $ ceiling (y1 - y0) + 10
    uuid <- UUID.toString <$> UUID.nextRandom
    let filename = "/tmp/" <> uuid <> ".svg"
    render filename w h $ do
        C.transform (C.Matrix 1 0 0 1 (5 - x0) (5 - y0))
        cairoScope (setColor white >> C.paint)
        sketch a
        setColor (mma 0)
        C.stroke
    traceM (msg <> ": " <> filename)
    pure b

traceSketchId :: (HasBoundingBox a, Sketch a) => String -> a -> a
traceSketchId msg a = traceSketch msg a a
