module Draw.Color.Schemes.Continuous
(
    -- * Linear, clamped on [0..1]

    -- ** Matplotlib
    --
    -- $matplotlibLink

      magma
    , inferno
    , plasma
    , viridis
    , cividis
    , turbo



    -- ** ColorBrewer2 sequential
    --
    -- $colorbrewerLink

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



    -- ** ColorBrewer2 divisive
    --
    -- $colorbrewerLink

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
    --
    -- $matplotlibLink
    , twilight
    , twilightShifted
)
where



import Draw.Color
import Draw.Color.Schemes.Internal.Common

import qualified Draw.Color.Schemes.Internal.ColorBrewer2 as ColorBrewer2
import qualified Draw.Color.Schemes.Internal.MatPlotLib   as MatPlotLib

-- $colorbrewerLink
--
-- https://colorbrewer2.org/

-- $matplotlibLink
--
-- https://matplotlib.org/



-- | Matplotlib’s magma color scheme.
--
-- <<docs/colors/schemes/continuous/matplotlib/magma.png>>
magma :: Double -> Color Double
magma = toColor . clamped MatPlotLib.magma

-- | Matplotlib’s inferno color scheme.
--
-- <<docs/colors/schemes/continuous/matplotlib/inferno.png>>
inferno :: Double -> Color Double
inferno = toColor . clamped MatPlotLib.inferno

-- | Matplotlib’s plasma color scheme.
--
-- <<docs/colors/schemes/continuous/matplotlib/plasma.png>>
plasma :: Double -> Color Double
plasma = toColor . clamped MatPlotLib.plasma

-- | Matplotlib’s viridis color scheme.
--
-- <<docs/colors/schemes/continuous/matplotlib/viridis.png>>
viridis :: Double -> Color Double
viridis = toColor . clamped MatPlotLib.viridis

-- | Matplotlib’s cividis color scheme.
--
-- <<docs/colors/schemes/continuous/matplotlib/cividis.png>>
cividis :: Double -> Color Double
cividis = toColor . clamped MatPlotLib.cividis

-- | Matplotlib’s turbo color scheme.
--
-- <<docs/colors/schemes/continuous/matplotlib/turbo.png>>
turbo :: Double -> Color Double
turbo = toColor . clamped MatPlotLib.turbo

-- | Matplotlib’s twilight color scheme.
--
-- <<docs/colors/schemes/continuous/matplotlib/twilight.png>>
twilight :: Double -> Color Double
twilight = toColor . cyclic MatPlotLib.twilight

-- | Matplotlib’s shifted twilight color scheme.
--
-- <<docs/colors/schemes/continuous/matplotlib/twilightShifted.png>>
twilightShifted :: Double -> Color Double
twilightShifted = toColor . cyclic MatPlotLib.twilightShifted



-- | Color Brewer 2’s sequential OrRd scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/orRd.png>>
orRd :: Double -> Color Double
orRd = toColor . clamped ColorBrewer2.sequential_OrRd

-- | Color Brewer 2’s sequential PuBu scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/puBu.png>>
puBu :: Double -> Color Double
puBu = toColor . clamped ColorBrewer2.sequential_PuBu

-- | Color Brewer 2’s sequential BuPu scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/buPu.png>>
buPu :: Double -> Color Double
buPu = toColor . clamped ColorBrewer2.sequential_BuPu

-- | Color Brewer 2’s sequential Oranges scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/oranges.png>>
oranges :: Double -> Color Double
oranges = toColor . clamped ColorBrewer2.sequential_Oranges

-- | Color Brewer 2’s sequential BuGn scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/buGn.png>>
buGn :: Double -> Color Double
buGn = toColor . clamped ColorBrewer2.sequential_BuGn

-- | Color Brewer 2’s sequential YlOrBr scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/ylOrBr.png>>
ylOrBr :: Double -> Color Double
ylOrBr = toColor . clamped ColorBrewer2.sequential_YlOrBr

-- | Color Brewer 2’s sequential YlGn scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/ylGn.png>>
ylGn :: Double -> Color Double
ylGn = toColor . clamped ColorBrewer2.sequential_YlGn

-- | Color Brewer 2’s sequential Reds scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/reds.png>>
reds :: Double -> Color Double
reds = toColor . clamped ColorBrewer2.sequential_Reds

-- | Color Brewer 2’s sequential RdPu scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/rdPu.png>>
rdPu :: Double -> Color Double
rdPu = toColor . clamped ColorBrewer2.sequential_RdPu

-- | Color Brewer 2’s sequential Greens scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/greens.png>>
greens :: Double -> Color Double
greens = toColor . clamped ColorBrewer2.sequential_Greens

-- | Color Brewer 2’s sequential YlGnBu scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/ylGnBu.png>>
ylGnBu :: Double -> Color Double
ylGnBu = toColor . clamped ColorBrewer2.sequential_YlGnBu

-- | Color Brewer 2’s sequential Purples scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/purples.png>>
purples :: Double -> Color Double
purples = toColor . clamped ColorBrewer2.sequential_Purples

-- | Color Brewer 2’s sequential GnBu scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/gnBu.png>>
gnBu :: Double -> Color Double
gnBu = toColor . clamped ColorBrewer2.sequential_GnBu

-- | Color Brewer 2’s sequential Greys scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/greys.png>>
greys :: Double -> Color Double
greys = toColor . clamped ColorBrewer2.sequential_Greys

-- | Color Brewer 2’s sequential YlOrRd scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/ylOrRd.png>>
ylOrRd :: Double -> Color Double
ylOrRd = toColor . clamped ColorBrewer2.sequential_YlOrRd

-- | Color Brewer 2’s sequential PuRd scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/puRd.png>>
puRd :: Double -> Color Double
puRd = toColor . clamped ColorBrewer2.sequential_PuRd

-- | Color Brewer 2’s sequential Blues scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/blues.png>>
blues :: Double -> Color Double
blues = toColor . clamped ColorBrewer2.sequential_Blues

-- | Color Brewer 2’s sequential PuBuGn scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/puBuGn.png>>
puBuGn :: Double -> Color Double
puBuGn = toColor . clamped ColorBrewer2.sequential_PuBuGn

-- | Color Brewer 2’s divisive Spectral scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/spectral.png>>
spectral :: Double -> Color Double
spectral = toColor . clamped ColorBrewer2.divisive_Spectral

-- | Color Brewer 2’s divisive RdYlGn scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/rdYlGn.png>>
rdYlGn :: Double -> Color Double
rdYlGn = toColor . clamped ColorBrewer2.divisive_RdYlGn

-- | Color Brewer 2’s divisive RdBu scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/rdBu.png>>
rdBu :: Double -> Color Double
rdBu = toColor . clamped ColorBrewer2.divisive_RdBu

-- | Color Brewer 2’s divisive PiYG scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/piYG.png>>
piYG :: Double -> Color Double
piYG = toColor . clamped ColorBrewer2.divisive_PiYG

-- | Color Brewer 2’s divisive PRGn scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/pRGn.png>>
pRGn :: Double -> Color Double
pRGn = toColor . clamped ColorBrewer2.divisive_PRGn

-- | Color Brewer 2’s divisive RdYlBu scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/rdYlBu.png>>
rdYlBu :: Double -> Color Double
rdYlBu = toColor . clamped ColorBrewer2.divisive_RdYlBu

-- | Color Brewer 2’s divisive BrBG scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/brBG.png>>
brBG :: Double -> Color Double
brBG = toColor . clamped ColorBrewer2.divisive_BrBG

-- | Color Brewer 2’s divisive RdGy scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/rdGy.png>>
rdGy :: Double -> Color Double
rdGy = toColor . clamped ColorBrewer2.divisive_RdGy

-- | Color Brewer 2’s divisive PuOr scheme
--
-- <<docs/colors/schemes/continuous/colorbrewer2/puOr.png>>
puOr :: Double -> Color Double
puOr = toColor . clamped ColorBrewer2.divisive_PuOr
