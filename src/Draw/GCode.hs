{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw.GCode (
    renderGCode
) where



import           Data.Foldable
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Vector    (Vector)
import qualified Data.Vector    as V
import           Formatting hiding (center)
import           Geometry.Core



decimal :: Format r (Double -> r)
decimal = fixed 3

withLoweredPen :: Vector GCode -> Vector GCode
withLoweredPen codes = [GRelativeMovement, GMoveZ (-0.5)] <> codes <> [GRelativeMovement, GMoveZ 0.5]

data GCode
    = GComment Text
    | GAbsoluteMovement
    | GRelativeMovement
    | GRapidMovement
    | GLinearMovement
    | GFeedrate Double
    | GArcCw Vec2 Vec2 -- ^ IJ, XY
    | GArcCcw Vec2 Vec2
    | GMoveXY Vec2
    | GMoveZ Double

renderGCodeElement :: GCode -> Text
renderGCodeElement = \case
    GComment comment              -> "; " <> comment
    GRapidMovement                -> "G0 ; Linear interpolation; rapid movement"
    GLinearMovement               -> "G1 ; Linear interpolation; move with feed rate"
    GMoveXY (Vec2 x y)            -> format ("X" % decimal % " Y" %  decimal) x y
    GMoveZ z                      -> format ("Z" % decimal) z
    GFeedrate f                   -> format ("F" % decimal) f
    GArcCw (Vec2 i j) (Vec2 x y)  -> format ("G2 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j
    GArcCcw (Vec2 i j) (Vec2 x y) -> format ("G3 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j
    GAbsoluteMovement             -> "G90 ; abs0lute movement"
    GRelativeMovement             -> "G91 ; re1ative movement"

renderGCode :: Vector GCode -> Text
renderGCode = T.unlines . V.toList . fmap renderGCodeElement

class ToGCode a where
    toGCode :: a -> Vector GCode

instance ToGCode Circle where
    toGCode (Circle center radius) =
        [GComment "Move to left of the circle", GLinearMovement, GRapidMovement, GMoveXY (center -. Vec2 radius 0)]
        <> withLoweredPen [GAbsoluteMovement, GArcCw (Vec2 radius 0) (center +. Vec2 radius 0), GArcCw (Vec2 radius 0) (center +. Vec2 radius 0)]


instance {-# OVERLAPPING #-} Sequential f => ToGCode (f Vec2) where
    toGCode = go . toList
      where
        go [] = mempty :: Vector GCode
        go (x:ys) =
            [ GComment "{ Polyline", GRapidMovement, GAbsoluteMovement, GMoveXY x]
            <>
            withLoweredPen ([GAbsoluteMovement] <> V.fromList [ GMoveXY xy | xy <- ys ])
            <>
            [ GComment "} End polyline" ]

instance {-# OVERLAPPABLE #-} (Sequential f, ToGCode a) => ToGCode (f a) where
    toGCode = foldMap toGCode
