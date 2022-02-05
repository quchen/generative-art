module Draw.Color.Schemes.Internal.Haskell (
    logo
) where



import           Data.Vector (Vector)
import qualified Data.Vector as V

import Draw.Color.Schemes.Internal.Common



-- | Official Haskell colors, as extracted from the SVG logo on haskell.org.
logo :: Vector RGB
logo = V.fromList [dark, medium, bright]
  where
    dark   = rgbFF 0x45 0x3a 0x62
    medium = rgbFF 0x5e 0x50 0x86
    bright = rgbFF 0x8f 0x4e 0x8b

rgbFF :: Int -> Int -> Int -> RGB
rgbFF r g b = RGB (fromIntegral r / 256) (fromIntegral g / 256) (fromIntegral b / 256)
