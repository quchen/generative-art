module Main where



import Data.List.Extended (nubOrd)
import qualified Data.Vector.Unboxed as V
import Data.Traversable
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Text.TrueType as TT
import System.Random.MWC

import Draw
import Geometry
import Geometry.Algorithms.Sampling



main :: IO ()
main = do
    Right iosevka <- TT.loadFontFile "/home/fthoma/.nix-profile/share/fonts/truetype/iosevka-custom-regular.ttf"
    gen <- create
    let params = PoissonDiscParams
            { _poissonShape = boundingBox [Vec2 100 50, Vec2 900 850]
            , _poissonRadius = 80
            , _poissonK = 3
            }
    pts <- poissonDisc gen params
    glyphs <- for pts $ \pt -> do
        char <- uniformRM ('a', 'z') gen
        style <- uniformM gen
        pure (char, pt, style)
    render "out/typography.png" 1000 1000 $ do
        cairoScope (setColor white >> C.paint)
        C.setLineWidth 1
        C.translate (-50) 100
        for_ glyphs $ \(char, Vec2 x y, style) -> cairoScope $ do
            C.translate x y
            case style of
                Outline -> sketch (fst <$> glyphOutline iosevka 200 char)
                Hatched angle -> sketch (hatchedGlyph iosevka 200 char angle 5)
            C.stroke

data GlyphStyle
    = Outline
    | Hatched Angle
    deriving (Eq, Show)

instance Uniform GlyphStyle where
    uniformM gen = uniformRM (0, 1 :: Int) gen >>= \case
        0 -> pure Outline
        1 -> Hatched . deg <$> uniformRM (0, 180) gen

glyph :: TT.Font -> Double -> Char -> [Polygon]
glyph font size c = fmap (Polygon . fmap toVec2 . nubOrd . V.toList) polys
  where
    dpi = 96
    pt = TT.pixelSizeInPointAtDpi (realToFrac size) dpi
    toVec2 (x, y) = Vec2 (realToFrac x) (realToFrac y)
    [polys] = TT.getGlyphForStrings dpi [(font, pt, [c])]

str :: TT.Font -> Double -> String -> [[Polygon]]
str font size t = fmap (fmap (Polygon . fmap toVec2 . nubOrd . V.toList)) glyphs
  where
    dpi = 96
    pt = TT.pixelSizeInPointAtDpi (realToFrac size) dpi
    toVec2 (x, y) = Vec2 (realToFrac x) (realToFrac y)
    glyphs = TT.getStringCurveAtPoint dpi (0, 0) [(font, pt, t)]

hatchedGlyph
    :: TT.Font
    -> Double -- ^ Font size
    -> Char -- ^ Glyph
    -> Angle -- ^ Direction in which the lines will point. @'deg' 0@ is parallel to the x axis.
    -> Double -- ^ Distance between shading lines
    -> [Line]
hatchedGlyph font size c angle hatchInterval = do
    let polygons = glyphOutline font size c
    let polygonsAligned = fmap (\(p, ioh) -> (transform (rotate (negateV angle)) p, ioh)) polygons
    horizontalScissors <- do
        let BoundingBox (Vec2 xLo yLo) (Vec2 xHi yHi) = boundingBox (fst <$> polygonsAligned)
        y <- takeWhile (< yHi) (tail (iterate (+ hatchInterval) yLo))
        pure (Line (Vec2 xLo y) (Vec2 xHi y))
    positiveHatches <-
        [ line
        | (polygonAligned, Island) <- polygonsAligned
        , (line, LineInsidePolygon) <- clipPolygonWithLine polygonAligned horizontalScissors
        ]
    horizontalHatches <- foldl' (\ls (poly, _) -> [line | (line, LineOutsidePolygon) <- ls >>= clipPolygonWithLineSegment poly]) [positiveHatches] (filter ((== Hole) . snd) polygonsAligned)
    pure (transform (rotate angle) horizontalHatches)

glyphOutline :: TT.Font -> Double -> Char -> [(Polygon, IslandOrHole)]
glyphOutline font size c = foldl' combinePolygons [p] ps
  where
    rawPolygons = glyph font size c
    classify poly = case polygonOrientation poly of
        PolygonPositive -> (poly, Island)
        PolygonNegative -> (poly, Hole)
    p:ps = classify <$> rawPolygons
    combinePolygons :: [(Polygon, IslandOrHole)] -> (Polygon, IslandOrHole) -> [(Polygon, IslandOrHole)]
    combinePolygons ps (p, ioh) = case ioh of
        Island -> unionsPP ps p
        Hole   -> differencesPP ps p

unionsPP :: [(Polygon, IslandOrHole)] -> Polygon -> [(Polygon, IslandOrHole)]
unionsPP [] p = [(p, Island)]
unionsPP ps p =
    [ q'
    | (q, ioh) <- ps
    , q' <- case ioh of
        Island -> unionPP q p
        Hole   -> differencePP q p >>= \case
            (x, Island) -> [(x, Hole)]
            (x, Hole)   -> [(x, Island)]
    ]

differencesPP :: [(Polygon, IslandOrHole)] -> Polygon -> [(Polygon, IslandOrHole)]
differencesPP [] _ = []
differencesPP ps p =
    [ q'
    | (q, ioh) <- ps
    , q' <- case ioh of
        Island -> differencePP q p
        Hole   -> unionPP q p >>= \case
            (x, Island) -> [(x, Hole)]
            (x, Hole)   -> [(x, Island)]
    ]
