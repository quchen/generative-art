module Draw.Color.Schemes.Discrete (
    -- * Mathematica
    mathematica97

    -- * Color Brewer 2
    , set2
    , accent
    , set1
    , set3
    , dark2
    , paired
    , pastel2
    , pastel1
) where



import qualified Data.Vector as V
import Data.Vector (Vector, (!), (!?))

import Draw.Color
import Draw.Color.Schemes.Common

import qualified Draw.Color.Schemes.RawColorLists.ColorBrewer2 as ColorBrewer2



-- | Mathematica’s ColorData[97] scheme, which I find really nice. Please don’t sue
-- me Steven o:-)
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

-- | Color Brewer 2’s qualitative Set2 palette
set2 :: Int -> Color Double
set2 = toColor . discreteCyclic ColorBrewer2.qualitative_Set2

-- | Color Brewer 2’s qualitative Accent palette
accent :: Int -> Color Double
accent = toColor . discreteCyclic ColorBrewer2.qualitative_Accent

-- | Color Brewer 2’s qualitative Set1 palette
set1 :: Int -> Color Double
set1 = toColor . discreteCyclic ColorBrewer2.qualitative_Set1

-- | Color Brewer 2’s qualitative Set3 palette
set3 :: Int -> Color Double
set3 = toColor . discreteCyclic ColorBrewer2.qualitative_Set3

-- | Color Brewer 2’s qualitative Dark2 palette
dark2 :: Int -> Color Double
dark2 = toColor . discreteCyclic ColorBrewer2.qualitative_Dark2

-- | Color Brewer 2’s qualitative Paired palette
paired :: Int -> Color Double
paired = toColor . discreteCyclic ColorBrewer2.qualitative_Paired

-- | Color Brewer 2’s qualitative Pastel2 palette
pastel2 :: Int -> Color Double
pastel2 = toColor . discreteCyclic ColorBrewer2.qualitative_Pastel2

-- | Color Brewer 2’s qualitative Pastel1 palette
pastel1 :: Int -> Color Double
pastel1 = toColor . discreteCyclic ColorBrewer2.qualitative_Pastel1
