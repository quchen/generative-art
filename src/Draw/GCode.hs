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
withLoweredPen codes =
    [G91_RelativeMovement, G00_LinearRapidMovement 0 0 (-0.5)]
    <> codes
    <> [G91_RelativeMovement, G00_LinearRapidMovement 0 0 0.5]

data GCode
    = GComment Text
    | G90_AbsoluteMovement
    | G91_RelativeMovement
    | G00_LinearRapidMovement Double Double Double
    | G01_LinearMovement Double Double Double
    | F_Feedrate Double
    | G02_ArcClockwise Vec2 Vec2 -- ^ IJ, XY
    | G03_ArcCounterClockwise Vec2 Vec2

renderGCodeElement :: GCode -> Text
renderGCodeElement = \case
    GComment comment                             -> "; " <> comment

    G00_LinearRapidMovement x y z                 -> format ("G0 X" % decimal % " Y" % decimal % " Z" % decimal) x y z
    G01_LinearMovement x y z                      -> format ("G1 X" % decimal % " Y" % decimal % " Z" % decimal) x y z
    G02_ArcClockwise (Vec2 i j) (Vec2 x y)        -> format ("G2 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j
    G03_ArcCounterClockwise (Vec2 i j) (Vec2 x y) -> format ("G3 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j
    G90_AbsoluteMovement                         -> "G90 ; abs0lute movement"
    G91_RelativeMovement                         -> "G91 ; re1ative movement"

    F_Feedrate f                                 -> format ("F" % decimal) f

renderGCode :: Vector GCode -> Text
renderGCode = T.unlines . V.toList . fmap renderGCodeElement

class ToGCode a where
    toGCode :: a -> Vector GCode

instance ToGCode Circle where
    toGCode (Circle center radius) =
        [GComment "Circle", G01_LinearMovement, G00_LinearRapidMovement, GMoveXY (center -. Vec2 radius 0)]
        <> withLoweredPen [G90_AbsoluteMovement, G02_ArcClockwise (Vec2 radius 0) (center +. Vec2 radius 0), G02_ArcClockwise (Vec2 radius 0) (center +. Vec2 radius 0)]


instance {-# OVERLAPPING #-} Sequential f => ToGCode (f Vec2) where
    toGCode = go . toList
      where
        go [] = mempty :: Vector GCode
        go (x:ys) =
            [ GComment "{ Polyline", G00_LinearRapidMovement, G90_AbsoluteMovement, GMoveXY x]
            <>
            withLoweredPen ([G90_AbsoluteMovement] <> V.fromList [ GMoveXY xy | xy <- ys ])
            <>
            [ GComment "} End polyline" ]

instance {-# OVERLAPPABLE #-} (Sequential f, ToGCode a) => ToGCode (f a) where
    toGCode = foldMap toGCode
