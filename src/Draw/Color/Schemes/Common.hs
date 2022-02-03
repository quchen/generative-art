module Draw.Color.Schemes.Common where



import qualified Data.Vector as V
import Data.Vector (Vector, (!))

import Draw.Color
import Numerics.Interpolation



data RGB = RGB !Double !Double !Double
    deriving (Eq, Ord, Show)

toColor :: RGB -> Color Double
toColor (RGB r g b) = rgb r g b

clamp :: Ord a => a -> a -> a -> a
clamp a b x =
    let lo = min a b
        hi = max a b
    in min hi (max x lo)

clamped :: Vector RGB -> Double -> RGB
clamped = linearColorInterpolation (\nColors ix -> clamp 0 (nColors-1) ix)

cyclic :: Vector RGB -> Double -> RGB
cyclic = linearColorInterpolation (\nColors ix -> mod ix nColors)

discreteCyclic :: Vector RGB -> Int -> RGB
discreteCyclic xs i = xs ! mod i (V.length xs)

-- | Pick a color from a list of colors, interpolating linearly between neighbours
-- of we hit the color between two others.
linearColorInterpolation
    :: (Int -> Int -> Int) -- ^ Given an index and the number of colors, which actual
                           --   vector index should be used? See e.g. 'clamped'.
    -> Vector RGB          -- ^ Color data
    -> Double              -- ^ Value to pick color for
    -> RGB
linearColorInterpolation picker xs = \query ->
    let nColors = V.length xs

        indexContinuous = linearInterpolate (0,1) (0, fromIntegral nColors - 1) query
        indexLo = floor indexContinuous
        indexHi = ceiling indexContinuous

        RGB rLo gLo bLo = xs ! picker nColors indexLo
        RGB rHi gHi bHi = xs ! picker nColors indexHi

        [r,g,b] = do
            (channelLo, channelHi) <- [(rLo, rHi), (gLo, gHi), (bLo, bHi)]
            pure $ case compare channelLo channelHi of
                EQ -> channelLo
                _  -> linearInterpolate
                    (fromIntegral indexLo, fromIntegral indexHi)
                    (channelLo, channelHi)
                    indexContinuous
    in RGB r g b
