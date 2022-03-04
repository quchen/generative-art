-- | Beautiful and practical color schemes, taken from
--
--  * Color Brewer 2: https://colorbrewer2.org/
--  * Matplotlib: https://matplotlib.org/
--  * Seaborn: https://seaborn.pydata.org/
module Draw.Color.Schemes.Continuous
(
    -- * Linear, clamped on [0..1]
    -- ** Haskell
      haskell

    -- ** Matplotlib
    , magma
    , inferno
    , plasma
    , viridis
    , cividis
    , turbo

    -- ** Seaborn sequential
    , rocket
    , mako
    , flare
    , crest


    -- ** Seaborn diverging
    , vlag
    , icefire



    -- ** ColorBrewer2 sequential
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
)
where



import Draw.Color
import Draw.Color.Schemes.Internal.Common

import qualified Draw.Color.Schemes.Internal.ColorBrewer2 as ColorBrewer2
import qualified Draw.Color.Schemes.Internal.Haskell      as Haskell
import qualified Draw.Color.Schemes.Internal.MatPlotLib   as MatPlotLib
import qualified Draw.Color.Schemes.Internal.Seaborn      as Seaborn



-- | Official Haskell colors, as extracted from the SVG logo on haskell.org,
-- linearly interpolated between.
--
-- The actual logo’s colors can be found at @[0, 0.5, 1]@.
haskell :: Double -> Color Double
haskell = toColor . clamped Haskell.logo

-- | <<docs/colors/schemes/continuous/matplotlib/magma.png>>
magma :: Double -> Color Double
magma = toColor . clamped MatPlotLib.magma

-- | <<docs/colors/schemes/continuous/matplotlib/inferno.png>>
inferno :: Double -> Color Double
inferno = toColor . clamped MatPlotLib.inferno

-- | <<docs/colors/schemes/continuous/matplotlib/plasma.png>>
plasma :: Double -> Color Double
plasma = toColor . clamped MatPlotLib.plasma

-- | <<docs/colors/schemes/continuous/matplotlib/viridis.png>>
viridis :: Double -> Color Double
viridis = toColor . clamped MatPlotLib.viridis

-- | <<docs/colors/schemes/continuous/matplotlib/cividis.png>>
cividis :: Double -> Color Double
cividis = toColor . clamped MatPlotLib.cividis

-- | <<docs/colors/schemes/continuous/matplotlib/turbo.png>>
turbo :: Double -> Color Double
turbo = toColor . clamped MatPlotLib.turbo

-- | <<docs/colors/schemes/continuous/matplotlib/twilight.png>>
twilight :: Double -> Color Double
twilight = toColor . cyclic MatPlotLib.twilight

-- | <<docs/colors/schemes/continuous/seaborn/rocket.png>>
rocket :: Double -> Color Double
rocket = toColor . clamped Seaborn.rocket

-- | <<docs/colors/schemes/continuous/seaborn/mako.png>>
mako :: Double -> Color Double
mako = toColor . clamped Seaborn.mako

-- | <<docs/colors/schemes/continuous/seaborn/flare.png>>
flare :: Double -> Color Double
flare = toColor . clamped Seaborn.flare

-- | <<docs/colors/schemes/continuous/seaborn/crest.png>>
crest :: Double -> Color Double
crest = toColor . clamped Seaborn.crest

-- | <<docs/colors/schemes/continuous/seaborn/vlag.png>>
vlag :: Double -> Color Double
vlag = toColor . clamped Seaborn.vlag
-- | <<docs/colors/schemes/continuous/seaborn/icefire.png>>
icefire :: Double -> Color Double
icefire = toColor . clamped Seaborn.icefire

-- | <<docs/colors/schemes/continuous/colorbrewer2/orRd.png>>
orRd :: Double -> Color Double
orRd = toColor . clamped ColorBrewer2.sequential_OrRd

-- | <<docs/colors/schemes/continuous/colorbrewer2/puBu.png>>
puBu :: Double -> Color Double
puBu = toColor . clamped ColorBrewer2.sequential_PuBu

-- | <<docs/colors/schemes/continuous/colorbrewer2/buPu.png>>
buPu :: Double -> Color Double
buPu = toColor . clamped ColorBrewer2.sequential_BuPu

-- | <<docs/colors/schemes/continuous/colorbrewer2/oranges.png>>
oranges :: Double -> Color Double
oranges = toColor . clamped ColorBrewer2.sequential_Oranges

-- | <<docs/colors/schemes/continuous/colorbrewer2/buGn.png>>
buGn :: Double -> Color Double
buGn = toColor . clamped ColorBrewer2.sequential_BuGn

-- | <<docs/colors/schemes/continuous/colorbrewer2/ylOrBr.png>>
ylOrBr :: Double -> Color Double
ylOrBr = toColor . clamped ColorBrewer2.sequential_YlOrBr

-- | <<docs/colors/schemes/continuous/colorbrewer2/ylGn.png>>
ylGn :: Double -> Color Double
ylGn = toColor . clamped ColorBrewer2.sequential_YlGn

-- | <<docs/colors/schemes/continuous/colorbrewer2/reds.png>>
reds :: Double -> Color Double
reds = toColor . clamped ColorBrewer2.sequential_Reds

-- | <<docs/colors/schemes/continuous/colorbrewer2/rdPu.png>>
rdPu :: Double -> Color Double
rdPu = toColor . clamped ColorBrewer2.sequential_RdPu

-- | <<docs/colors/schemes/continuous/colorbrewer2/greens.png>>
greens :: Double -> Color Double
greens = toColor . clamped ColorBrewer2.sequential_Greens

-- | <<docs/colors/schemes/continuous/colorbrewer2/ylGnBu.png>>
ylGnBu :: Double -> Color Double
ylGnBu = toColor . clamped ColorBrewer2.sequential_YlGnBu

-- | <<docs/colors/schemes/continuous/colorbrewer2/purples.png>>
purples :: Double -> Color Double
purples = toColor . clamped ColorBrewer2.sequential_Purples

-- | <<docs/colors/schemes/continuous/colorbrewer2/gnBu.png>>
gnBu :: Double -> Color Double
gnBu = toColor . clamped ColorBrewer2.sequential_GnBu

-- | <<docs/colors/schemes/continuous/colorbrewer2/greys.png>>
greys :: Double -> Color Double
greys = toColor . clamped ColorBrewer2.sequential_Greys

-- | <<docs/colors/schemes/continuous/colorbrewer2/ylOrRd.png>>
ylOrRd :: Double -> Color Double
ylOrRd = toColor . clamped ColorBrewer2.sequential_YlOrRd

-- | <<docs/colors/schemes/continuous/colorbrewer2/puRd.png>>
puRd :: Double -> Color Double
puRd = toColor . clamped ColorBrewer2.sequential_PuRd

-- | <<docs/colors/schemes/continuous/colorbrewer2/blues.png>>
blues :: Double -> Color Double
blues = toColor . clamped ColorBrewer2.sequential_Blues

-- | <<docs/colors/schemes/continuous/colorbrewer2/puBuGn.png>>
puBuGn :: Double -> Color Double
puBuGn = toColor . clamped ColorBrewer2.sequential_PuBuGn

-- | <<docs/colors/schemes/continuous/colorbrewer2/spectral.png>>
spectral :: Double -> Color Double
spectral = toColor . clamped ColorBrewer2.divisive_Spectral

-- | <<docs/colors/schemes/continuous/colorbrewer2/rdYlGn.png>>
rdYlGn :: Double -> Color Double
rdYlGn = toColor . clamped ColorBrewer2.divisive_RdYlGn

-- | <<docs/colors/schemes/continuous/colorbrewer2/rdBu.png>>
rdBu :: Double -> Color Double
rdBu = toColor . clamped ColorBrewer2.divisive_RdBu

-- | <<docs/colors/schemes/continuous/colorbrewer2/piYG.png>>
piYG :: Double -> Color Double
piYG = toColor . clamped ColorBrewer2.divisive_PiYG

-- | <<docs/colors/schemes/continuous/colorbrewer2/pRGn.png>>
pRGn :: Double -> Color Double
pRGn = toColor . clamped ColorBrewer2.divisive_PRGn

-- | <<docs/colors/schemes/continuous/colorbrewer2/rdYlBu.png>>
rdYlBu :: Double -> Color Double
rdYlBu = toColor . clamped ColorBrewer2.divisive_RdYlBu

-- | <<docs/colors/schemes/continuous/colorbrewer2/brBG.png>>
brBG :: Double -> Color Double
brBG = toColor . clamped ColorBrewer2.divisive_BrBG

-- | <<docs/colors/schemes/continuous/colorbrewer2/rdGy.png>>
rdGy :: Double -> Color Double
rdGy = toColor . clamped ColorBrewer2.divisive_RdGy

-- | <<docs/colors/schemes/continuous/colorbrewer2/puOr.png>>
puOr :: Double -> Color Double
puOr = toColor . clamped ColorBrewer2.divisive_PuOr
