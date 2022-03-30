{-# LANGUAGE OverloadedStrings #-}

module Draw.Plotting.GCode (
      GCode(..)
    , renderGCode
) where



import           Data.List
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import           Formatting     hiding (center)



data GCode
    = GComment TL.Text
    | GBlock [GCode]
    | F_Feedrate Double
    | M0_Pause

    | G00_LinearRapidMove (Maybe Double) (Maybe Double) (Maybe Double) -- ^ G0 X Y Z
    | G01_LinearFeedrateMove (Maybe Double) (Maybe Double) (Maybe Double) -- ^ G1 X Y Z
    | G02_ArcClockwise Double Double Double Double -- ^ G02 I J X Y
    | G03_ArcCounterClockwise Double Double Double Double -- ^ G03 I J X Y
    | G04_Dwell Double
    | G28_GotoPredefinedPosition (Maybe Double) (Maybe Double) (Maybe Double) -- ^ G28 X Y Z
    | G30_GotoPredefinedPosition (Maybe Double) (Maybe Double) (Maybe Double) -- ^ G30 X Y Z
    | G90_AbsoluteMovement
    | G91_RelativeMovement

renderGCode :: [GCode] -> TL.Text
renderGCode [GBlock xs] = renderGCode xs -- Remove indentation if it's a single top-level block
renderGCode xs = TL.toLazyText (mconcat (intersperse "\n" (fmap (renderGcodeIndented 0) xs)))

renderGcodeIndented :: Int -> GCode -> TL.Builder
renderGcodeIndented !level = \case
    GComment comment -> indent ("; " <> TL.fromLazyText comment)
    GBlock content   -> mconcat (intersperse "\n" (map (renderGcodeIndented (level+1)) content))
    F_Feedrate f     -> indent (bformat ("F " % double) f)
    M0_Pause         -> indent "M0 ; Pause/wait for user input"

    G00_LinearRapidMove Nothing Nothing Nothing -> errorComment "G00 requires at least one coordinate argument; omitting empty G00"
    G00_LinearRapidMove x y z -> indent (bformat ("G0" % optional "X" % optional "Y" % optional "Z") x y z)

    G01_LinearFeedrateMove Nothing Nothing Nothing -> errorComment "G01 requires at least one coordinate argument; omitting empty G01"
    G01_LinearFeedrateMove x y z -> indent (bformat ("G1" % optional "X" % optional "Y" % optional "Z") x y z)

    G02_ArcClockwise        i j x y -> indent (bformat ("G2" % required "X" % required "Y" % required "I" % required "J") x y i j)
    G03_ArcCounterClockwise i j x y -> indent (bformat ("G3" % required "X" % required "Y" % required "I" % required "J") x y i j)

    G04_Dwell s -> indent (bformat ("G4" % required "P") s)

    G28_GotoPredefinedPosition x y z -> indent (bformat ("G28" % optional "X" % optional "Y" % optional "Z") x y z)
    G30_GotoPredefinedPosition x y z -> indent (bformat ("G30" % optional "X" % optional "Y" % optional "Z") x y z)

    G90_AbsoluteMovement -> indent "G90 ; G9(0) => abs(0)lute movement"
    G91_RelativeMovement -> indent "G91 ; G9(1) => re(1)ative movement"
  where
    indentation = "    "
    indent x = TL.fromLazyText (TL.replicate (fromIntegral level) indentation) <> x
    errorComment msg = renderGcodeIndented level (GComment ("ERROR: " <> msg))

-- | Required number. Example: G02 requires an I parameter, hence @required "X"@.
required :: TL.Builder -> Format r (Double -> r)
required x = " " % now x % double

-- | Optional number. Example: G01 does not require a Z position, hence @optional "Z"@.
optional :: TL.Builder -> Format r (Maybe Double -> r)
optional = optioned . required

double :: Format r (Double -> r)
double = let decimals = 3 in fixed decimals
