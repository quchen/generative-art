module Main where



import qualified Data.Vector.Unboxed as V
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Text.TrueType as TT

import Draw
import Geometry



main :: IO ()
main = do
    Right iosevka <- TT.loadFontFile "/home/fthoma/.nix-profile/share/fonts/truetype/iosevka-custom-regular.ttf"
    print (TT.descriptorOf iosevka)
    render "out/typography.png" 200 200 $ do
        C.translate (-10) 168
        cairoScope (setColor white >> C.paint)
        let [f] = glyph iosevka 200 'f'
        sketch f
        C.stroke
        C.translate 100 0
        let fHatched = hatch f (deg 45) 5 ++ hatch f (deg (-45)) 5
        for_ fHatched sketch
        C.stroke

glyph :: TT.Font -> Double -> Char -> [Polygon]
glyph font size c = fmap (Polygon . fmap toVec2 . V.toList) polys
  where
    dpi = 96
    pt = TT.pixelSizeInPointAtDpi (realToFrac size) dpi
    toVec2 (x, y) = Vec2 (realToFrac x) (realToFrac y)
    [polys] = TT.getGlyphForStrings dpi [(font, pt, [c])]

str :: TT.Font -> Double -> String -> [[Polygon]]
str font size t = fmap (fmap (Polygon . fmap toVec2 . V.toList)) glyphs
  where
    dpi = 96
    pt = TT.pixelSizeInPointAtDpi (realToFrac size) dpi
    toVec2 (x, y) = Vec2 (realToFrac x) (realToFrac y)
    glyphs = TT.getStringCurveAtPoint dpi (0, 0) [(font, pt, t)]


