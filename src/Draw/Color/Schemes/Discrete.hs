module Draw.Color.Schemes.Discrete (
    mathematica97
) where



import Data.Vector ((!))

import Draw.Color
import Draw.Color.Schemes.Common
import Draw.Color.Schemes.RawColorLists



-- | Mathematica’s ColorData[97] scheme, which I find really nice. Please don’t sue
-- me Steven o:-)
mathematica97 :: Int -> Color Double
mathematica97 i
    -- Note that Mathematica *does* allow values of 0, yielding a red-orange
    -- color out of the interpolated section! It starts plots with 1 though.
    | i < 0 = error ("mathematica97 color index out of bounds: " ++ show i)
mathematica97 i
    | i < 10 = toColor (mma97_constantBeginning ! i)
         -- MMA uses 1-based indexing, hence < instead of <= here.
mathematica97 i =
    let (_, i') = properFraction (fromIntegral (i-10) / goldenRatio)
                ------------------------------- ^^^^
                -- Offset of -1 missing because of Haskell’s 0-based
                -- vs. Mathematica’s 1-based indexing.
        goldenRatio = (1 + sqrt 5)/2
    in toColor (cyclic mma97_interpolating i')
