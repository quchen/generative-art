module Draw.Color.Schemes.Internal.Common (
      RGB(..)
    , rgbFF
    , toColor
    , clamped
    , cyclic
    , discreteCyclic
) where



import qualified Data.Vector as V
import Data.Vector (Vector, (!))

import Draw.Color
import Numerics.Interpolation



data RGB = RGB !Double !Double !Double
    deriving (Eq, Ord, Show)

toColor :: RGB -> Color Double
toColor (RGB r g b) = rgb r g b

-- | Simple way to enter hex-based RGB codes.
--
-- @'rgbFF' 0xab 0xcd 0xef@ is equivalent to @#0xabcdef@.
rgbFF :: Int -> Int -> Int -> RGB
rgbFF r g b = RGB (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)

-- | Constrain a number between two values: find the closest value within an
-- interval specified by its boundary points.
--
-- >>> clamp 0 1 0.5
-- 0.5
--
-- >>> clamp 0 1 999
-- 1
-- >>> clamp 1 0 999
-- 1
-- >>> clamp 1 0 (-999)
-- 0
clamp
    :: Ord a
    => a -- ^ First interval boundary
    -> a -- ^ Second interval boundary
    -> a -- ^ Input value
    -> a -- ^ Closest value in the interval
clamp a b x =
    let lo = min a b
        hi = max a b
    in min hi (max x lo)

-- | Pick a color from a continuous set, stopping at the beginning or end when the
-- query is out of bounds. When picking colors between the scheme’s values,
-- interpolate between them.
clamped :: Vector RGB -> Double -> RGB
clamped = linearColorInterpolation (\nColors ix -> clamp 0 (nColors-1) ix)

-- | Pick a color from a continuous set, starting from the beginning again once
-- reaching the end. When picking colors between the scheme’s values, interpolate
-- between them.
cyclic :: Vector RGB -> Double -> RGB
cyclic = linearColorInterpolation (\nColors ix -> mod ix nColors)

-- | Pick a color from a discrete set, starting from the beginning again once reaching the end.
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
