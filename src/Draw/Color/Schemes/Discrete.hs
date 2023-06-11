-- | Beautiful and practical color schemes, taken from
--
--  * Mathematica: https://www.wolfram.com/mathematica/
--  * Color Brewer 2: https://colorbrewer2.org/
--
-- +-----------------+--------------------------------------------------------------+---------+
-- | Name            |                                                              | Domain  |
-- +=================+==============================================================+=========+
-- | 'mathematica97' | <<docs/colors/schemes/discrete/mathematica/ColorData97.svg>> | [0..∞)  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'accent'        | <<docs/colors/schemes/discrete/colorbrewer2/accent.svg>>     | [0..7]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'dark2'         | <<docs/colors/schemes/discrete/colorbrewer2/dark2.svg>>      | [0..7]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'paired'        | <<docs/colors/schemes/discrete/colorbrewer2/paired.svg>>     | [0..11] |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'pastel1'       | <<docs/colors/schemes/discrete/colorbrewer2/pastel1.svg>>    | [0..8]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'pastel2'       | <<docs/colors/schemes/discrete/colorbrewer2/pastel2.svg>>    | [0..7]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'set1'          | <<docs/colors/schemes/discrete/colorbrewer2/set1.svg>>       | [0..8]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'set2'          | <<docs/colors/schemes/discrete/colorbrewer2/set2.svg>>       | [0..7]  |
-- +-----------------+--------------------------------------------------------------+---------+
-- | 'set3'          | <<docs/colors/schemes/discrete/colorbrewer2/set3.svg>>       | [0..11] |
-- +-----------------+--------------------------------------------------------------+---------+
module Draw.Color.Schemes.Discrete (
    -- * Mathematica
      mathematica97

    -- * Color Brewer 2
    , accent
    , dark2
    , paired
    , pastel1
    , pastel2
    , set1
    , set2
    , set3
) where



import           Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V

import Draw.Color
import Draw.Color.Schemes.Internal.Common

import qualified Draw.Color.Schemes.Internal.ColorBrewer2 as ColorBrewer2



-- | Mathematica’s ColorData[97] scheme, which I find really nice. Please don’t sue
-- me Steven o:-)
--
-- <<docs/colors/schemes/discrete/mathematica/ColorData97.svg>>
mathematica97 :: Int -> Color Double
mathematica97 i = case mma97cache !? i of
    Just cached -> cached
    Nothing -> toColor (generateMathematica97 i)

-- Taken straight out of mathematica, translated to Haskell: @ColorData[97] // FullForm@
generateMathematica97 :: Int -> RGB
generateMathematica97 i
    -- Note that Mathematica *does* allow values of 0, yielding a red-orange
    -- color out of the interpolated section! It starts plots with 1 though.
    | i < 0 = error ("mathematica97 color index out of bounds: " ++ show i)
generateMathematica97 i
    | i < 10 = mma97_constantBeginning ! i
         -- MMA uses 1-based indexing, hence < instead of <= here.
generateMathematica97 i =
    let (_, i') = properFraction (fromIntegral (i-10) / goldenRatio)
                ------------------------------- ^^^^
                -- Offset of -1 missing because of Haskell’s 0-based
                -- vs. Mathematica’s 1-based indexing.
        goldenRatio = (1 + sqrt 5)/2
    in cyclic mma97_interpolating i'

-- For frequent access, we cache the first entries. If later entries are needed,
-- chances are they’re not reused anyway, so why cache them?
mma97cache :: Vector (Color Double)
mma97cache = V.generate 32 (\i -> toColor (generateMathematica97 i))

-- | The first 10 color values are hardcoded and not interpolated
mma97_constantBeginning :: Vector RGB
mma97_constantBeginning = V.fromList
    [ RGB 0.368417 0.506779 0.709798
    , RGB 0.880722 0.611041 0.142051
    , RGB 0.560181 0.691569 0.194885
    , RGB 0.922526 0.385626 0.209179
    , RGB 0.528488 0.470624 0.701351
    , RGB 0.772079 0.431554 0.102387
    , RGB 0.363898 0.618501 0.782349
    , RGB 1        0.75     0
    , RGB 0.647624 0.37816  0.614037
    , RGB 0.571589 0.586483 0
    ]

-- Starting at the 11th value, Mathematica starts picking intermediate colors
-- between a short lookup table
mma97_interpolating :: Vector RGB
mma97_interpolating = V.fromList
    [ RGB 0.915  0.3325 0.2125
    , RGB 0.83   0.46   0
    , RGB 0.9575 0.545  0.11475
    , RGB 1      0.7575 0
    , RGB 0.6175 0.715  0
    , RGB 0.15   0.715  0.595
    , RGB 0.3625 0.545  0.85
    , RGB 0.575  0.4175 0.85
    , RGB 0.677  0.358  0.595
    , RGB 0.7875 0.358  0.425
    , RGB 0.915  0.3325 0.2125
    ]

-- | <<docs/colors/schemes/discrete/colorbrewer2/accent.svg>>
accent :: Int -> Color Double
accent = toColor . discreteCyclic ColorBrewer2.qualitative_Accent

-- | <<docs/colors/schemes/discrete/colorbrewer2/set1.svg>>
set1 :: Int -> Color Double
set1 = toColor . discreteCyclic ColorBrewer2.qualitative_Set1

-- | <<docs/colors/schemes/discrete/colorbrewer2/set2.svg>>
set2 :: Int -> Color Double
set2 = toColor . discreteCyclic ColorBrewer2.qualitative_Set2

-- | <<docs/colors/schemes/discrete/colorbrewer2/set3.svg>>
set3 :: Int -> Color Double
set3 = toColor . discreteCyclic ColorBrewer2.qualitative_Set3

-- | <<docs/colors/schemes/discrete/colorbrewer2/dark2.svg>>
dark2 :: Int -> Color Double
dark2 = toColor . discreteCyclic ColorBrewer2.qualitative_Dark2

-- | <<docs/colors/schemes/discrete/colorbrewer2/paired.svg>>
paired :: Int -> Color Double
paired = toColor . discreteCyclic ColorBrewer2.qualitative_Paired

-- | <<docs/colors/schemes/discrete/colorbrewer2/pastel2.svg>>
pastel2 :: Int -> Color Double
pastel2 = toColor . discreteCyclic ColorBrewer2.qualitative_Pastel2

-- | <<docs/colors/schemes/discrete/colorbrewer2/pastel1.svg>>
pastel1 :: Int -> Color Double
pastel1 = toColor . discreteCyclic ColorBrewer2.qualitative_Pastel1
