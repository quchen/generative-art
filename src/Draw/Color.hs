-- | Utilities to manipulate colors, and setting them for Cairo drawings.
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
, parseRgbaHex
, parseRgbHex
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
import qualified Graphics.Rendering.Cairo as C



-- | Anything we can instruct Cairo to set its color to.
class CairoColor color where
    setColor :: color -> C.Render ()

instance Real a => CairoColor (Colour a) where
    setColor = uncurryRGB C.setSourceRGB . toSRGB . colourConvert

instance (Real a, Floating a) => CairoColor (AlphaColour a) where
    setColor color = uncurryRGB C.setSourceRGBA
        (toSRGB (colourConvert (dissolve (1/alpha) color `over` black)))
        (realToFrac alpha)
      where alpha = alphaChannel color

-- | American English type synonym
type Color a = Colour a

-- | American English type synonym
type AlphaColor a = AlphaColour a

-- | Convert a color from HSL space
hsl :: Double -- ^ Hue [0..360]
    -> Double -- ^ Saturation [0..1]
    -> Double -- ^ Lightness [0..1]
    -> Color Double
hsl h s l = uncurryRGB (rgbUsingSpace sRGBSpace) (Colour.hsl h s l)

-- | Convert a color from HSLA space
hsla:: Double -- ^ Hue [0..360]
    -> Double -- ^ Saturation [0..1]
    -> Double -- ^ Lightness [0..1]
    -> Double -- ^ Alpha [0..1]
    -> AlphaColor Double
hsla h s l a = Draw.Color.hsl h s l `withOpacity` a

-- | Convert a color from HSV space
hsv :: Double -- ^ Hue [0..360]
    -> Double -- ^ Saturation [0..1]
    -> Double -- ^ Value (~ brightness) [0..1]
    -> Color Double
hsv h s v = uncurryRGB (rgbUsingSpace sRGBSpace) (Colour.hsv h s v)

-- | Convert a color from HSVA space
hsva:: Double -- ^ Hue [0..360]
    -> Double -- ^ Saturation [0..1]
    -> Double -- ^ Value (~ brightness) [0..1]
    -> Double -- ^ Alpha [0..1]
    -> AlphaColor Double
hsva h s v a = Draw.Color.hsv h s v `withOpacity` a

-- | Convert a color from sRGB space
rgb :: Double  -- ^ Red [0..1]
    -> Double  -- ^ Green [0..1]
    -> Double  -- ^ Blue [0..1]
    -> Color Double
rgb = sRGB

-- | Convert a color from sRGBA space
rgba:: Double -- ^ Red [0..1]
    -> Double -- ^ Green [0..1]
    -> Double -- ^ Blue [0..1]
    -> Double -- ^ Alpha [0..1]
    -> AlphaColor Double
rgba r g b a = sRGB r g b `withOpacity` a

-- | Parse a RGBA hex value. 'error's on bad input, so be careful!
--
-- @
-- 'parseRgbaHex' "0x123456ab"
-- '=='
-- 'rgba' ('fromIntegral' 0x12/255) ('fromIntegral' 0x34/255) ('fromIntegral' 0x56/255) ('fromIntegral' 0xab/255)
-- @
parseRgbaHex :: String -> AlphaColor Double
parseRgbaHex ('#' : rrggbbaa) = parseRgbaHex rrggbbaa
parseRgbaHex [r1, r2, g1, g2, b1, b2, a1, a2] = rgba
    (read ("0x" ++ [r1, r2]) / 255)
    (read ("0x" ++ [g1, g2]) / 255)
    (read ("0x" ++ [b1, b2]) / 255)
    (read ("0x" ++ [a1, a2]) / 255)
parseRgbaHex [r1, r2, g1, g2, b1, b2] = parseRgbaHex [r1, r2, g1, g2, b1, b2, 'f', 'f']
parseRgbaHex str = error ("parseRgbaHex: cannot parse " ++ str)

-- | Parse a RGB hex value. 'error's on bad input, so be careful!
--
-- @
-- 'parseRgbHex' "0x123456"
-- '=='
-- 'rgb' ('fromIntegral' 0x12/255) ('fromIntegral' 0x34/255) ('fromIntegral' 0x56/255)
-- @
parseRgbHex :: String -> Color Double
parseRgbHex ('#' : rrggbb) = parseRgbHex rrggbb
parseRgbHex [r1, r2, g1, g2, b1, b2] = rgb
    (read ("0x" ++ [r1, r2]) / 255)
    (read ("0x" ++ [g1, g2]) / 255)
    (read ("0x" ++ [b1, b2]) / 255)
parseRgbHex str = error ("parseRgbHex: cannot parse " ++ str)

average :: [Color Double] -> Color Double
average colors = mconcat (darken (1/fromIntegral (length colors)) <$> colors)

-- | Adjust a HSV value per component.
adjustHsv
    :: (Double -> Double) -- ^ Adjust Hue [0..360]
    -> (Double -> Double) -- ^ Adjust Saturation [0..1]
    -> (Double -> Double) -- ^ Adjust Value (~ Brightness) [0..1]
    -> Color Double -> Color Double
adjustHsv fh fs fv color = hsv (fh h) (fs s) (fv v)
  where (h, s, v) = Colour.hsvView (toSRGB color)

-- | Adjust a HSL value per component.
adjustHsl
    :: (Double -> Double) -- ^ Adjust Hue [0..360]
    -> (Double -> Double) -- ^ Adjust Saturation [0..1]
    -> (Double -> Double) -- ^ Adjust Luminance [0..1]
    -> Color Double -> Color Double
adjustHsl fh fs fl color = uncurryRGB (rgbUsingSpace sRGBSpace) (Colour.hsl (fh h) (fs s) (fl l))
  where (h, s, l) = Colour.hslView (toSRGB color)
