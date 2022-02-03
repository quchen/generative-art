module Draw.Color.Schemes.Continuous
(
    -- * Linear, clamped on [0..1]

    -- ** Matplotlib
      magma
    , inferno
    , plasma
    , viridis
    , cividis
    , turbo

    -- ** ColorBrewer2 monotonous
    , orRd
    , puBu
    , buPu
    , oranges
    , buGn
    , ylOrBr
    , ylGn
    , reds
    , rdPu
    , greens
    , ylGnBu
    , purples
    , gnBu
    , greys
    , ylOrRd
    , puRd
    , blues
    , puBuGn

    -- ** ColorBrewer2 diverging
    , spectral
    , rdYlGn
    , rdBu
    , piYG
    , pRGn
    , rdYlBu
    , brBG
    , rdGy
    , puOr

    -- * Cyclic on [0..1]
    , twilight
    , twilightShifted
)
where



import Draw.Color
import Draw.Color.Schemes.Common

import qualified Draw.Color.Schemes.RawColorLists.ColorBrewer2 as ColorBrewer2
import qualified Draw.Color.Schemes.RawColorLists.MatPlotLib   as MatPlotLib



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



-- | Color Brewer 2’s sequential OrRd scheme
orRd :: Double -> Color Double
orRd = toColor . clamped ColorBrewer2.sequential_OrRd

-- | Color Brewer 2’s sequential PuBu scheme
puBu :: Double -> Color Double
puBu = toColor . clamped ColorBrewer2.sequential_PuBu

-- | Color Brewer 2’s sequential BuPu scheme
buPu :: Double -> Color Double
buPu = toColor . clamped ColorBrewer2.sequential_BuPu

-- | Color Brewer 2’s sequential Oranges scheme
oranges :: Double -> Color Double
oranges = toColor . clamped ColorBrewer2.sequential_Oranges

-- | Color Brewer 2’s sequential BuGn scheme
buGn :: Double -> Color Double
buGn = toColor . clamped ColorBrewer2.sequential_BuGn

-- | Color Brewer 2’s sequential YlOrBr scheme
ylOrBr :: Double -> Color Double
ylOrBr = toColor . clamped ColorBrewer2.sequential_YlOrBr

-- | Color Brewer 2’s sequential YlGn scheme
ylGn :: Double -> Color Double
ylGn = toColor . clamped ColorBrewer2.sequential_YlGn

-- | Color Brewer 2’s sequential Reds scheme
reds :: Double -> Color Double
reds = toColor . clamped ColorBrewer2.sequential_Reds

-- | Color Brewer 2’s sequential RdPu scheme
rdPu :: Double -> Color Double
rdPu = toColor . clamped ColorBrewer2.sequential_RdPu

-- | Color Brewer 2’s sequential Greens scheme
greens :: Double -> Color Double
greens = toColor . clamped ColorBrewer2.sequential_Greens

-- | Color Brewer 2’s sequential YlGnBu scheme
ylGnBu :: Double -> Color Double
ylGnBu = toColor . clamped ColorBrewer2.sequential_YlGnBu

-- | Color Brewer 2’s sequential Purples scheme
purples :: Double -> Color Double
purples = toColor . clamped ColorBrewer2.sequential_Purples

-- | Color Brewer 2’s sequential GnBu scheme
gnBu :: Double -> Color Double
gnBu = toColor . clamped ColorBrewer2.sequential_GnBu

-- | Color Brewer 2’s sequential Greys scheme
greys :: Double -> Color Double
greys = toColor . clamped ColorBrewer2.sequential_Greys

-- | Color Brewer 2’s sequential YlOrRd scheme
ylOrRd :: Double -> Color Double
ylOrRd = toColor . clamped ColorBrewer2.sequential_YlOrRd

-- | Color Brewer 2’s sequential PuRd scheme
puRd :: Double -> Color Double
puRd = toColor . clamped ColorBrewer2.sequential_PuRd

-- | Color Brewer 2’s sequential Blues scheme
blues :: Double -> Color Double
blues = toColor . clamped ColorBrewer2.sequential_Blues

-- | Color Brewer 2’s sequential PuBuGn scheme
puBuGn :: Double -> Color Double
puBuGn = toColor . clamped ColorBrewer2.sequential_PuBuGn

-- | Color Brewer 2’s divisive Spectral scheme
spectral :: Double -> Color Double
spectral = toColor . clamped ColorBrewer2.divisive_Spectral

-- | Color Brewer 2’s divisive RdYlGn scheme
rdYlGn :: Double -> Color Double
rdYlGn = toColor . clamped ColorBrewer2.divisive_RdYlGn

-- | Color Brewer 2’s divisive RdBu scheme
rdBu :: Double -> Color Double
rdBu = toColor . clamped ColorBrewer2.divisive_RdBu

-- | Color Brewer 2’s divisive PiYG scheme
piYG :: Double -> Color Double
piYG = toColor . clamped ColorBrewer2.divisive_PiYG

-- | Color Brewer 2’s divisive PRGn scheme
pRGn :: Double -> Color Double
pRGn = toColor . clamped ColorBrewer2.divisive_PRGn

-- | Color Brewer 2’s divisive RdYlBu scheme
rdYlBu :: Double -> Color Double
rdYlBu = toColor . clamped ColorBrewer2.divisive_RdYlBu

-- | Color Brewer 2’s divisive BrBG scheme
brBG :: Double -> Color Double
brBG = toColor . clamped ColorBrewer2.divisive_BrBG

-- | Color Brewer 2’s divisive RdGy scheme
rdGy :: Double -> Color Double
rdGy = toColor . clamped ColorBrewer2.divisive_RdGy

-- | Color Brewer 2’s divisive PuOr scheme
puOr :: Double -> Color Double
puOr = toColor . clamped ColorBrewer2.divisive_PuOr
