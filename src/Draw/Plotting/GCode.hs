{-# LANGUAGE OverloadedStrings #-}

module Draw.Plotting.GCode (
      GCode(..)
    , renderGCode
) where



import qualified Data.Text.Lazy as TL
import           Formatting     hiding (center)

decimal :: Format r (Double -> r)
decimal = fixed 3

data GCode
    = GComment TL.Text
    | GBlock [GCode]
    | F_Feedrate Double
    | M0_Pause

    | G00_LinearRapidMove (Maybe Double) (Maybe Double) (Maybe Double) -- ^ G0 X Y Z
    | G01_LinearFeedrateMove (Maybe Double) (Maybe Double) (Maybe Double) -- ^ G1 X Y Z
    | G02_ArcClockwise Double Double Double Double -- ^ G02 I J X Y
    | G03_ArcCounterClockwise Double Double Double Double -- ^ G03 I J X Y
    | G90_AbsoluteMovement
    | G91_RelativeMovement

renderGCode :: [GCode] -> TL.Text
renderGCode [GBlock xs] = renderGCode xs -- Remove indentation if it's a single top-level block
renderGCode xs = TL.intercalate "\n" (fmap (renderGcodeIndented 0) xs)

renderGcodeIndented :: Int -> GCode -> TL.Text
renderGcodeIndented !level = \case
    GComment comment -> indent ("; " <> comment)
    GBlock content   -> TL.intercalate "\n" (map (renderGcodeIndented (level+1)) content)
    F_Feedrate f     -> indent (format ("F " % decimal) f)
    M0_Pause         -> indent "M0 ; Pause/wait for user input"

    G00_LinearRapidMove Nothing Nothing Nothing -> mempty
    G00_LinearRapidMove x y z                   -> indent (format ("G0" % optioned (" X"%decimal) % optioned (" Y"%decimal) % optioned (" Z"%decimal)) x y z)

    G01_LinearFeedrateMove Nothing Nothing Nothing -> mempty
    G01_LinearFeedrateMove x y z                   -> indent (format ("G1" % optioned (" X"%decimal) % optioned (" Y"%decimal) % optioned (" Z"%decimal)) x y z)

    G02_ArcClockwise        i j x y -> indent (format ("G2 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j)
    G03_ArcCounterClockwise i j x y -> indent (format ("G3 X" % decimal % " Y" % decimal % " I" % decimal % " J" % decimal) x y i j)

    G90_AbsoluteMovement -> indent "G90"
    G91_RelativeMovement -> indent "G91"
  where
    indentation = "    "
    indent x = TL.replicate (fromIntegral level) indentation <> x
