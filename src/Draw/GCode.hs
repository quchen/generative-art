{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Draw.GCode where


import           Data.Foldable
import           Data.Text.Lazy (Text)
import           Data.Vector    (Vector)
import qualified Data.Vector    as V
import           Formatting
import           Geometry.Core



decimal :: Format r (Double -> r)
decimal = fixed 3

withLoweredPen :: Vector GCode -> Vector GCode
withLoweredPen codes = [GMoveAbsoluteZ (-0.5)] <> codes <> [GMoveAbsoluteZ 0.5]

data GCode
    = GComment Text
    | GDeclareAbsoluteMovement
    | GDeclareRelativeMovement
    | GMoveAbsoluteXY Vec2
    | GMoveAbsoluteZ Double

renderGCode :: GCode -> Text
renderGCode = \case
    GComment comment           -> "; " <> comment
    GDeclareAbsoluteMovement   -> "G90 ; absolute movement"
    GDeclareRelativeMovement   -> "G91 ; relative movement"
    GMoveAbsoluteXY (Vec2 x y) -> format ("G1 X" % decimal % " Y" %  decimal) x y
    GMoveAbsoluteZ z           -> format ("G1 Z" % decimal) z

class ToGCode a where
    toGCode :: a -> Vector GCode

instance {-# OVERLAPPING #-} Sequential f => ToGCode (f Vec2) where
    toGCode = go . toList
      where
        go [] = mempty :: Vector GCode
        go (start:xys) =
            [ GComment "{ Polyline"
            , GMoveAbsoluteXY start ]
            <>
            withLoweredPen (V.fromList [ GMoveAbsoluteXY xy | xy <- xys ])
            <>
            [ GComment " } End polyline" ]

instance {-# OVERLAPPABLE #-} (Sequential f, ToGCode a) => ToGCode (f a) where
    toGCode = foldMap toGCode
