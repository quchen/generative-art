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
import qualified Draw.Color.Schemes.RawColorLists as Raw



-- | Python’s Matplotlib’s magma color scheme.
magma :: Double -> Color Double
magma = toColor . clamped Raw.magma

-- | Python’s Matplotlib’s inferno color scheme.
inferno :: Double -> Color Double
inferno = toColor . clamped Raw.inferno

-- | Python’s Matplotlib’s plasma color scheme.
plasma :: Double -> Color Double
plasma = toColor . clamped Raw.plasma

-- | Python’s Matplotlib’s viridis color scheme.
viridis :: Double -> Color Double
viridis = toColor . clamped Raw.viridis

-- | Python’s Matplotlib’s cividis color scheme.
cividis :: Double -> Color Double
cividis = toColor . clamped Raw.cividis

turbo :: Double -> Color Double
turbo = toColor . clamped Raw.turbo


-- | Python’s Matplotlib’s twilight color scheme.
twilight :: Double -> Color Double
twilight = toColor . cyclic Raw.twilight

-- | Python’s Matplotlib’s shifted twilight color scheme.
twilightShifted :: Double -> Color Double
twilightShifted = toColor . cyclic Raw.twilightShifted
