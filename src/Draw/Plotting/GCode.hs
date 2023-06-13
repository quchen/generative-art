{-# LANGUAGE OverloadedStrings #-}

module Draw.Plotting.GCode (
      GCode(..)
    , renderGCode
) where



import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TL
import           Formatting             hiding (center)



-- | Raw GCode for penplotting.
data GCode
    = GComment TL.Text
    | GBlock [GCode]    -- ^ Group a couple of commands for easier reading.
    | F_Feedrate Double -- ^ Set the feedrate. Normally mm/min, but can be altered using G93, G94, G20, G21.
    | M0_Pause          -- ^ Pause and wait for user input

    | G00_LinearRapidMove (Maybe Double) (Maybe Double) (Maybe Double)                   -- ^ @G0 X Y Z@: Move as fast as possible
    | G01_LinearFeedrateMove (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) -- ^ @G1 F X Y Z@: Move with specified feedrate
    | G02_ArcClockwise (Maybe Double) Double Double Double Double                        -- ^ @G2 F I J X Y@: Paint part of a circle, clockwise
    | G03_ArcCounterClockwise (Maybe Double) Double Double Double Double                 -- ^ @G3 F I J X Y@: Paint part of a circle, counterclockwise
    | G04_Dwell_ms Double -- ^ @G4 P@: wait a number of milliseconds.
    | G17_Plane_XY
    | G18_Plane_ZX
    | G19_Plane_YZ
    | G20_UseInches
    | G21_UseMm
    | G28_GotoPredefinedPosition (Maybe Double) (Maybe Double) (Maybe Double) -- ^ @G28 X Y Z@
    | G30_GotoPredefinedPosition (Maybe Double) (Maybe Double) (Maybe Double) -- ^ @G30 X Y Z@
    | G90_AbsoluteMovement -- ^ Move commands use coordinates relative to the origin
    | G91_RelativeMovement -- ^ Move commands use coordinates relative to the current position
    | G93_Feedrate_TravelInFractionOfMinute
    | G94_Feedrate_UnitsPerMinute
    deriving (Eq, Ord, Show)

-- | Convert 'GCode' to 'TL.Text', to be written to a file.
renderGCode :: [GCode] -> TL.Text
renderGCode [GBlock xs] = renderGCode xs -- Remove indentation if it's a single top-level block
renderGCode xs = TL.toLazyText (mconcat (intersperse "\n" (fmap (renderGcodeIndented 0) xs)))

renderGcodeIndented :: Int -> GCode -> TL.Builder
renderGcodeIndented !level = \case
    GComment comment -> indent ("; " <> TL.fromLazyText comment)
    GBlock content   -> mconcat (intersperse "\n" (map (renderGcodeIndented (level+1)) content))
    F_Feedrate f     -> indent (bformat ("F " % double) f)
    M0_Pause         -> indent "M0 (Pause/wait for user input)"

    G00_LinearRapidMove x y z
        | all isNothing [x,y,z] -> errorComment "G0 requires at least one coordinate argument; omitting empty G0"
        | otherwise -> indent (bformat ("G0" % optional "X" % optional "Y" % optional "Z") x y z)

    G01_LinearFeedrateMove f x y z
        | all isNothing [x,y,z] -> errorComment "G1 requires at least one coordinate argument; omitting empty G1"
        | otherwise -> indent (bformat ("G1" % optional "F" % optional "X" % optional "Y" % optional "Z") f x y z)

    G02_ArcClockwise        f i j x y -> indent (bformat ("G2" % optional "F" % required "X" % required "Y" % required "I" % required "J") f x y i j)
    G03_ArcCounterClockwise f i j x y -> indent (bformat ("G3" % optional "F" % required "X" % required "Y" % required "I" % required "J") f x y i j)

    G04_Dwell_ms s -> indent (bformat ("G4" % required "P") s)

    G17_Plane_XY -> indent "G17 (Use XY plane)"
    G18_Plane_ZX -> indent "G18 (Use ZX plane)"
    G19_Plane_YZ -> indent "G19 (Use YZ plane)"
    G20_UseInches -> error "Inches aren’t going to happen here, sorry."
    G21_UseMm -> indent "G21 (Use mm)"

    G28_GotoPredefinedPosition x y z -> indent (bformat ("G28" % optional "X" % optional "Y" % optional "Z") x y z)
    G30_GotoPredefinedPosition x y z -> indent (bformat ("G30" % optional "X" % optional "Y" % optional "Z") x y z)

    G90_AbsoluteMovement -> indent "G90 ;(G9(0) => abs(0)lute movement)"
    G91_RelativeMovement -> indent "G91 ;(G9(1) => re(1)ative movement)"

    G93_Feedrate_TravelInFractionOfMinute -> indent "G93 (feedrate is time to travel in fractions of one minute: F1000 = make the move in 60/1000 min)"
    G94_Feedrate_UnitsPerMinute -> indent "G94 (feedrate is units per minute: F1000 = move at 1000 mm/min)"
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
