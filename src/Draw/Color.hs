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



import           Data.Colour
import qualified Data.Colour              as ReExport hiding (black)
import           Data.Colour.Names        hiding (grey)
import           Data.Colour.RGBSpace     as Colour
import qualified Data.Colour.RGBSpace.HSL as Colour
import qualified Data.Colour.RGBSpace.HSV as Colour
import           Data.Colour.SRGB         as Colour
import qualified Graphics.Rendering.Cairo as C
import           Text.Read



-- $setup
-- >>> import Draw
-- >>> import Geometry.Core
-- >>> import qualified Graphics.Rendering.Cairo as C



-- | Anything we can instruct Cairo to set its color to.
class CairoColor color where
    -- |
    -- >>> :{
    -- haddockRender "Draw/Color/set_color.svg" 140 40 $ do
    --     for_ (zip [0..] [30, 40 .. 150-30]) $ \(i, x) -> do
    --         setColor (mma i)
    --         sketch (Circle (Vec2 x 20) 10)
    --         C.fill
    -- :}
    -- Generated file: size 4KB, crc32: 0xe0e16234
    --
    -- <<docs/haddock/Draw/Color/set_color.svg>>
    setColor :: color -> C.Render ()
    setColor = setColour

    setColour :: color -> C.Render ()
    setColour = setColor

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
parseRgbaHex [r1, r2, g1, g2, b1, b2, a1, a2] = rightOrError "parseRgbaHex" $ rgba
    <$> parseHexPair r1 r2
    <*> parseHexPair g1 g2
    <*> parseHexPair b1 b2
    <*> parseHexPair a1 a2
parseRgbaHex str
    | length str < 8 = error ("parseRgbaHex: input too short: " ++ str)
    | otherwise      = error ("parseRgbaHex: input too long: " ++ str)

-- | Parse a RGB hex value. 'error's on bad input, so be careful!
--
-- @
-- 'parseRgbHex' "0x123456"
-- '=='
-- 'rgb' ('fromIntegral' 0x12/255) ('fromIntegral' 0x34/255) ('fromIntegral' 0x56/255)
-- @
parseRgbHex :: String -> Color Double
parseRgbHex ('#' : rrggbb) = parseRgbHex rrggbb
parseRgbHex [r1, r2, g1, g2, b1, b2] = rightOrError "parseRgbHex" $ rgb
    <$> parseHexPair r1 r2
    <*> parseHexPair g1 g2
    <*> parseHexPair b1 b2
parseRgbHex str
    | length str < 6 = error ("parseRgbHex: input too short: " ++ str)
    | otherwise      = error ("parseRgbHex: input too long: " ++ str)

rightOrError :: String -> Either String rgb -> rgb
rightOrError _ (Right r) = r
rightOrError source (Left err) = error (source ++ ": " ++ err)

parseHexPair :: Char -> Char -> Either String Double
parseHexPair a b = case readMaybe ("0x" ++ [a,b]) of
    Just r -> Right (r / 255)
    Nothing -> Left ("Cannot parse hex pair: " ++ [a,b])

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
