module Draw.Color.Schemes.Discrete (
    mathematica97
) where



import qualified Data.Vector as V
import Data.Vector (Vector, (!))

import Draw.Color
import qualified Draw.Color.Schemes.Raw as Raw



pickCyclic :: Raw.Discrete (Vector a) -> Int -> a
pickCyclic (Raw.Discrete vec) = \index ->
    let nColors = V.length vec
    in vec ! mod index nColors

-- | Mathematica’s ColorData[97] scheme, which I find really nice. Please don’t sue
-- me Steven o:-)
mathematica97 :: Int -> Color Double
mathematica97 = Raw.toColor . pickCyclic Raw.mathematica97
