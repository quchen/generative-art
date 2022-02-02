module Draw.Color (
  Color
, AlphaColor
, CairoColor(..)
, rgb
, hsv
, hsl
, rgba
, hsva
, hsla
, parseRGBAHex
, parseRGBHex
, module ReExport
, average
, black
, white
, adjustHsl
, adjustHsv
) where

import qualified Data.Colour as ReExport hiding (black, darken)
import Data.Colour
import Data.Colour.Names hiding (grey)
import Data.Colour.RGBSpace as Colour
import qualified Data.Colour.RGBSpace.HSV as Colour
import qualified Data.Colour.RGBSpace.HSL as Colour
import Data.Colour.SRGB as Colour
import qualified Graphics.Rendering.Cairo as Cairo



class CairoColor color where
    setColor :: color -> Cairo.Render ()

instance Real a => CairoColor (Colour a) where
    setColor = uncurryRGB Cairo.setSourceRGB . toSRGB . colourConvert

instance (Real a, Floating a) => CairoColor (AlphaColour a) where
    setColor color = uncurryRGB Cairo.setSourceRGBA
        (toSRGB (colourConvert (dissolve (1/alpha) color `over` black)))
        (realToFrac alpha)
      where alpha = alphaChannel color

type Color a = Colour a
type AlphaColor a = AlphaColour a

-- | Convert a color from HSL space
hsl :: Double -- ^ Hue [0..360]
    -> Double -- ^ Saturation [0..1]
    -> Double -- ^ Lightness [0..1]
    -> Color Double
hsl h s l = uncurryRGB (rgbUsingSpace sRGBSpace) (Colour.hsl h s l)

-- | Convert a color from HSLA space
hsla :: Double -- ^ Hue [0..360]
    -> Double  -- ^ Saturation [0..1]
    -> Double  -- ^ Lightness [0..1]
    -> Double  -- ^ Alpha [0..1]
    -> AlphaColor Double
hsla h s l a = Draw.Color.hsl h s l `withOpacity` a

-- | Convert a color from HSV space
hsv :: Double -- ^ Hue [0..360]
    -> Double -- ^ Saturation [0..1]
    -> Double -- ^ Value (~ brightness) [0..1]
    -> Color Double
hsv h s v = uncurryRGB (rgbUsingSpace sRGBSpace) (Colour.hsv h s v)

-- | Convert a color from HSVA space
hsva :: Double -- ^ Hue [0..360]
    -> Double  -- ^ Saturation [0..1]
    -> Double  -- ^ Value (~ brightness) [0..1]
    -> Double  -- ^ Alpha [0..1]
    -> AlphaColor Double
hsva h s v a = Draw.Color.hsv h s v `withOpacity` a

rgb :: Double  -- ^ Red [0..1]
    -> Double  -- ^ Green [0..1]
    -> Double  -- ^ Blue [0..1]
    -> Color Double
rgb = sRGB

rgba :: Double -- ^ Red [0..1]
    -> Double  -- ^ Green [0..1]
    -> Double  -- ^ Blue [0..1]
    -> Double  -- ^ Alpha [0..1]
    -> AlphaColor Double
rgba r g b a = sRGB r g b `withOpacity` a

parseRGBAHex :: String -> AlphaColor Double
parseRGBAHex ('#' : rrggbbaa) = parseRGBAHex rrggbbaa
parseRGBAHex [r1, r2, g1, g2, b1, b2, a1, a2] = rgba
    (read ("0x" ++ [r1, r2]) / 255)
    (read ("0x" ++ [g1, g2]) / 255)
    (read ("0x" ++ [b1, b2]) / 255)
    (read ("0x" ++ [a1, a2]) / 255)
parseRGBAHex [r1, r2, g1, g2, b1, b2] = parseRGBAHex [r1, r2, g1, g2, b1, b2, 'f', 'f']
parseRGBAHex str = error ("parseRGBAHex: cannot parse " ++ str)

parseRGBHex :: String -> Color Double
parseRGBHex ('#' : rrggbb) = parseRGBHex rrggbb
parseRGBHex [r1, r2, g1, g2, b1, b2] = rgb
    (read ("0x" ++ [r1, r2]) / 255)
    (read ("0x" ++ [g1, g2]) / 255)
    (read ("0x" ++ [b1, b2]) / 255)
parseRGBHex str = error ("parseRGBHex: cannot parse " ++ str)

average :: [Color Double] -> Color Double
average colors = mconcat (darken (1/fromIntegral (length colors)) <$> colors)

adjustHsv :: (Double -> Double) -- ^ Adjust Hue [0..360]
    -> (Double -> Double) -- ^ Adjust Saturation [0..1]
    -> (Double -> Double) -- ^ Adjust Value (~ Brightness) [0..1]
    -> Color Double -> Color Double
adjustHsv fh fs fv color = hsv (fh h) (fs s) (fv v)
  where (h, s, v) = Colour.hsvView (toSRGB color)

adjustHsl :: (Double -> Double) -- ^ Adjust Hue [0..360]
    -> (Double -> Double) -- ^ Adjust Saturation [0..1]
    -> (Double -> Double) -- ^ Adjust Luminance [0..1]
    -> Color Double -> Color Double
adjustHsl fh fs fl color = uncurryRGB (rgbUsingSpace sRGBSpace) (Colour.hsl (fh h) (fs s) (fl l))
  where (h, s, l) = Colour.hslView (toSRGB color)
