module Main where



import Control.Monad
import Data.List.Extended (nubOrd)
import qualified Data.Vector.Unboxed as V
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Text.TrueType as TT
import System.Random.MWC

import Draw
import Geometry



main :: IO ()
main = do
    Right iosevka <- TT.loadFontFile "/home/fthoma/.nix-profile/share/fonts/truetype/iosevka-custom-regular.ttf"
    gen <- create
    glyphs <- replicateM 300 $ do
        pt <- uniformRM (Vec2 0 0, Vec2 1000 1000) gen
        char <- uniformRM ('a', 'z') gen
        angle <- deg <$> uniformRM (0, 360) gen
        pure $ transform (translate pt) $ hatchedGlyph iosevka 200 char angle 5
    render "out/typography.png" 1000 1000 $ do
        cairoScope (setColor white >> C.paint)
        C.setLineWidth 1
        C.translate (-50) 100
        for_ glyphs $ \g -> do
            for_ g sketch
            C.stroke

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
