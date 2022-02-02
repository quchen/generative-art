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



import qualified Data.Vector as V
import Data.Vector (Vector, (!))

import Numerics.Interpolation
import Draw.Color
import qualified Draw.Color.Schemes.Raw as Raw



clamp :: Ord a => a -> a -> a -> a
clamp a b x =
    let lo = min a b
        hi = max a b
    in min hi (max x lo)

clamped :: Raw.Continuous Raw.Clamped (Vector Raw.RGB) -> Double -> Raw.RGB
clamped (Raw.Continuous vec) = pickColor (\nColors ix -> clamp 0 (nColors-1) ix) vec

cyclic :: Raw.Continuous Raw.Cyclic (Vector Raw.RGB) -> Double -> Raw.RGB
cyclic (Raw.Continuous vec) = pickColor (\nColors ix -> mod ix nColors) vec

pickColor
    :: (Int -> Int -> Int) -- ^ Given an index and the number of colors, which actual
                           --   vector index should be used? See e.g. 'clamped'.
    -> Vector Raw.RGB      -- ^ Color data
    -> Double              -- ^ Value to pick color for
    -> Raw.RGB
pickColor picker xs = \query ->
    let nColors = V.length xs

        indexContinuous = linearInterpolate (0,1) (0, fromIntegral nColors - 1) query
        indexLo = floor indexContinuous
        indexHi = ceiling indexContinuous

        Raw.RGB rLo gLo bLo = xs ! picker nColors indexLo
        Raw.RGB rHi gHi bHi = xs ! picker nColors indexHi

        [r,g,b] = do
            (channelLo, channelHi) <- [(rLo, rHi), (gLo, gHi), (bLo, bHi)]
            pure $ case compare channelLo channelHi of
                EQ -> channelLo
                _  -> linearInterpolate
                    (fromIntegral indexLo, fromIntegral indexHi)
                    (channelLo, channelHi)
                    indexContinuous
    in Raw.RGB r g b

-- | Python’s Matplotlib’s magma color scheme.
--
-- <<file:///docs/colors/schemes/cividis.png>>
magma :: Double -> Color Double
magma = Raw.toColor . clamped Raw.magma

-- | Python’s Matplotlib’s inferno color scheme.
inferno :: Double -> Color Double
inferno = Raw.toColor . clamped Raw.inferno

-- | Python’s Matplotlib’s plasma color scheme.
plasma :: Double -> Color Double
plasma = Raw.toColor . clamped Raw.plasma

-- | Python’s Matplotlib’s viridis color scheme.
viridis :: Double -> Color Double
viridis = Raw.toColor . clamped Raw.viridis

-- | Python’s Matplotlib’s cividis color scheme.
cividis :: Double -> Color Double
cividis = Raw.toColor . clamped Raw.cividis

turbo :: Double -> Color Double
turbo = Raw.toColor . clamped Raw.turbo


-- | Python’s Matplotlib’s twilight color scheme.
twilight :: Double -> Color Double
twilight = Raw.toColor . cyclic Raw.twilight

-- | Python’s Matplotlib’s shifted twilight color scheme.
twilightShifted :: Double -> Color Double
twilightShifted = Raw.toColor . cyclic Raw.twilightShifted
