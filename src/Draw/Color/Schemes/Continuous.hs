module Draw.Color.Schemes.Continuous
(
    -- * Linear, clamped on [0..1]
      magma
    , inferno
    , plasma
    , viridis
    , cividis
    , turbo

    -- * Cyclic on [0..1]
    , twilight
    , twilightShifted
)
where



import Draw.Color
import Draw.Color.Schemes.Common
import qualified Draw.Color.Schemes.RawColorLists.MatPlotLib as MatPlotLib



-- | Python’s Matplotlib’s magma color scheme.
magma :: Double -> Color Double
magma = toColor . clamped MatPlotLib.magma

-- | Python’s Matplotlib’s inferno color scheme.
inferno :: Double -> Color Double
inferno = toColor . clamped MatPlotLib.inferno

-- | Python’s Matplotlib’s plasma color scheme.
plasma :: Double -> Color Double
plasma = toColor . clamped MatPlotLib.plasma

-- | Python’s Matplotlib’s viridis color scheme.
viridis :: Double -> Color Double
viridis = toColor . clamped MatPlotLib.viridis

-- | Python’s Matplotlib’s cividis color scheme.
cividis :: Double -> Color Double
cividis = toColor . clamped MatPlotLib.cividis

turbo :: Double -> Color Double
turbo = toColor . clamped MatPlotLib.turbo

-- | Python’s Matplotlib’s twilight color scheme.
twilight :: Double -> Color Double
twilight = toColor . cyclic MatPlotLib.twilight

-- | Python’s Matplotlib’s shifted twilight color scheme.
twilightShifted :: Double -> Color Double
twilightShifted = toColor . cyclic MatPlotLib.twilightShifted
