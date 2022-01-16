{-# LANGUAGE RecordWildCards #-}
module Draw.Color (
  setColorRGB
, setColorRGBA
, RGBA()
, RGB()
, rgb
, hsv
, rgba
, hsva
, mmaColor
, parseRGBAHex
, parseRGBHex
) where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import qualified Graphics.Rendering.Cairo as Cairo

data RGBA a = RGBA { _rgb :: !(RGB a), _alpha :: !a }

-- | Set the color to some RGBA value.
setColorRGBA :: RGBA Double -> Cairo.Render ()
setColorRGBA RGBA{..} = uncurryRGB Cairo.setSourceRGBA _rgb _alpha

-- | Set the color to some RGB value.
setColorRGB :: RGB Double -> Cairo.Render ()
setColorRGB = uncurryRGB Cairo.setSourceRGB

-- | Convert a color from HSVA space
hsva :: Double -- ^ Hue [0..360]
    -> Double  -- ^ Saturation [0..1]
    -> Double  -- ^ Value (~ brightness) [0..1]
    -> Double  -- ^ Alpha [0..1]
    -> RGBA Double
hsva h s v a = RGBA { _rgb = hsv h s v, _alpha = a }

rgb :: Double  -- ^ Red [0..1]
    -> Double  -- ^ Green [0..1]
    -> Double  -- ^ Blue [0..1]
    -> RGB Double
rgb = RGB

rgba :: Double  -- ^ Red [0..1]
    -> Double  -- ^ Green [0..1]
    -> Double  -- ^ Blue [0..1]
    -> Double  -- ^ Alpha [0..1]
    -> RGBA Double
rgba r g b a = RGBA { _rgb = rgb r g b, _alpha = a }

parseRGBAHex :: String -> RGBA Double
parseRGBAHex ('#' : rrggbbaa) = parseRGBAHex rrggbbaa
parseRGBAHex [r1, r2, g1, g2, b1, b2, a1, a2] = RGBA
    { _rgb = rgb
        (read ("0x" ++ [r1, r2]) / 255)
        (read ("0x" ++ [g1, g2]) / 255)
        (read ("0x" ++ [b1, b2]) / 255)
    , _alpha = read ("0x" ++ [a1, a2]) / 255
    }
parseRGBAHex [r1, r2, g1, g2, b1, b2] = parseRGBAHex [r1, r2, g1, g2, b1, b2, 'f', 'f']
parseRGBAHex _ = undefined

parseRGBHex :: String -> RGB Double
parseRGBHex ('#' : rrggbb) = parseRGBHex rrggbb
parseRGBHex [r1, r2, g1, g2, b1, b2] = rgb
    (read ("0x" ++ [r1, r2]) / 255)
    (read ("0x" ++ [g1, g2]) / 255)
    (read ("0x" ++ [b1, b2]) / 255)
parseRGBHex _ = undefined

-- | Mathematica’s ColorData[97] scheme.
mmaColor :: Int -> Double -> RGBA Double
mmaColor n alpha = rgba r g b alpha
  where
    (r,g,b) = case rem n 15 of
        0 ->  (0.368417, 0.506779, 0.709798)
        1 ->  (0.880722, 0.611041, 0.142051)
        2 ->  (0.560181, 0.691569, 0.194885)
        3 ->  (0.922526, 0.385626, 0.209179)
        4 ->  (0.528488, 0.470624, 0.701351)
        5 ->  (0.772079, 0.431554, 0.102387)
        6 ->  (0.363898, 0.618501, 0.782349)
        7 ->  (1, 0.75, 0)
        8 ->  (0.647624, 0.37816, 0.614037)
        9 ->  (0.571589, 0.586483, 0)
        10 -> (0.915, 0.3325, 0.2125)
        11 -> (0.40082222609352647, 0.5220066643438841, 0.85)
        12 -> (0.9728288904374106, 0.621644452187053, 0.07336199581899142)
        13 -> (0.736782672705901, 0.358, 0.5030266573755369)
        14 -> (0.28026441037696703, 0.715, 0.4292089322474965)
        _other -> error "modulus in mmaColor is broken"
