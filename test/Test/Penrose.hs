
{-# LANGUAGE RecordWildCards #-}
module Test.Penrose (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Cairo (Matrix(..))
import Graphics.Rendering.Cairo.SVG as Cairo

import Draw
import Geometry
import Penrose

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit
import Data.Traversable (for)

tests :: TestTree
tests = testGroup "Penrose tiling"
    [ testBaseConfigurations
    , testSubdivision
    , testInscribedPentagons
    , testSubdivisionWithInscribedPentagons
    , testSvgPrototiles
    ]

testBaseConfigurations :: TestTree
testBaseConfigurations = testCase "Base configurations" test
  where
    test = renderAllFormats 860 200 "test/out/penrose/1_base_configurations" $ do
        for_ (star1 (Vec2 100 100) 100) drawTileWithConnectors
        Cairo.translate 220 0
        for_ (star2 (Vec2 100 100) 100) drawTileWithConnectors
        Cairo.translate 220 0
        for_ (decagonRose (Vec2 100 100) 100) drawTileWithConnectors
        Cairo.translate 220 0
        for_ (asymmetricDecagon (Vec2 100 100) 100) drawTileWithConnectors

testSubdivision :: TestTree
testSubdivision = testCase "Subdividing base rhombs" test
  where
    fitToBox = scaleT 100 100
    baseRhombThick = transform fitToBox thickTileBase
    baseRhombThin = transform (translateT (Vec2 0 120) <> fitToBox) thinTileBase
    gen0 = baseRhombThick ++ baseRhombThin
    gen1 = subdivide =<< gen0
    gen2 = subdivide =<< gen1
    test = renderAllFormats 600 200 "test/out/penrose/2_subdivision" $ do
        Cairo.translate 10 10
        for_ gen0 drawTileWithConnectors
        Cairo.translate 150 0
        drawArrows
        Cairo.translate 50 0
        for_ gen1 drawTileWithConnectors
        Cairo.translate 150 0
        drawArrows
        Cairo.translate 50 0
        for_ gen2 drawTileWithConnectors
    drawArrows = do
        arrowSketch (Line (Vec2 0 50) (Vec2 30 50)) def >> stroke
        arrowSketch (Line (Vec2 20 150) (Vec2 50 150)) def >> stroke

testInscribedPentagons :: TestTree
testInscribedPentagons = testCase "Switch to pentagons & stars" test
  where
    test = renderAllFormats 200 200 "test/out/penrose/3_inscribed_pentagons" $
        for_ (decagonRose (Vec2 100 100) 100) drawInscribedPentagons

testSubdivisionWithInscribedPentagons :: TestTree
testSubdivisionWithInscribedPentagons = testCase "Subdivision rules with pentagons & stars" test
  where
    fitToBox = scaleT 100 100
    baseRhombThick = transform fitToBox thickTileBase
    baseRhombThin = transform (translateT (Vec2 0 120) <> fitToBox) thinTileBase
    gen0 = baseRhombThick ++ baseRhombThin
    gen1 = subdivide =<< gen0
    gen2 = subdivide =<< gen1
    test = renderAllFormats 600 200 "test/out/penrose/4_subdivision_with_pentagons" $ do
        Cairo.translate 10 10
        for_ gen0 drawInscribedPentagons
        Cairo.translate 150 0
        drawArrows
        Cairo.translate 50 0
        for_ gen1 drawInscribedPentagons
        Cairo.translate 150 0
        drawArrows
        Cairo.translate 50 0
        for_ gen2 drawInscribedPentagons
    drawArrows = do
        arrowSketch (Line (Vec2 0 50) (Vec2 30 50)) def >> stroke
        arrowSketch (Line (Vec2 20 150) (Vec2 50 150)) def >> stroke

testSvgPrototiles :: TestTree
testSvgPrototiles = testCase "Loading SVG prototiles" test
  where
    test = do
        prototiles <- svgProtoTiles
        for_ prototiles $ \(i, svg) ->
            renderAllFormats 1000 1000 ("test/out/penrose/5_prototiles_" ++ show i) $
                for_ (decagonRose (Vec2 500 500) 500 >>= subdivide >>= subdivide) $ renderProtoTile svg


renderProtoTile :: SVG -> Tile -> Render ()
renderProtoTile prototile t@Tile{..} = restoreStateAfter $ do
    polygonSketchOpen (asPolygon t)
    restoreStateAfter $ do
        Cairo.setSourceRGBA 1 0 0 0.5
        Cairo.setLineWidth 0.5
        strokePreserve
    clip
    let Distance unitLength = norm (tileP1 -. tileP0)
    case (tileType, polygonOrientation (asPolygon t)) of
        (Thin, PolygonPositive) -> do
            let Vec2 x y = tileP1 in Cairo.translate x y
            Cairo.rotate (getRad (angleOfLine (Line tileP2 tileP1)))
            Cairo.scale (unitLength / 100) (unitLength / 100)
            Cairo.rotate (-2 * getRad alpha)
            Cairo.translate (-100 * phi * cos (getRad alpha)) (-100 * phi * sin(getRad alpha))
        (Thin, PolygonNegative) -> do
            let Vec2 x y = tileP1 in Cairo.translate x y
            Cairo.rotate (getRad (angleOfLine (Line tileP1 tileP0)))
            Cairo.scale (unitLength / 100) (unitLength / 100)
            Cairo.rotate (-2 * getRad alpha)
            Cairo.translate (-100 * phi * cos (getRad alpha)) (100 * phi * sin(getRad alpha))
            cairoMirrorV
        (Thick, PolygonPositive) -> do
            let Vec2 x y = tileP2 in Cairo.translate x y
            Cairo.rotate (getRad (angleOfLine (Line tileP1 tileP0)))
            Cairo.scale (unitLength / 100) (unitLength / 100)
        (Thick, PolygonNegative) -> do
            let Vec2 x y = tileP2 in Cairo.translate x y
            Cairo.rotate (getRad (angleOfLine (Line tileP2 tileP1)))
            Cairo.scale (unitLength / 100) (unitLength / 100)
    let scaleFactor = 100 / 93.75 -- Why? I have no idea.
    Cairo.scale scaleFactor scaleFactor
    _ <- svgRender prototile
    pure ()

cairoMirrorV :: Render ()
cairoMirrorV = Cairo.transform (Cairo.Matrix 1 0 0 (-1) 0 0)


svgProtoTiles :: IO [(Int, SVG)]
svgProtoTiles = do
    let files = [1..3]
    for files $ \fileIndex -> do
        svg <- svgNewFromFile ("prototiles" ++ show fileIndex ++ ".svg")
        pure (fileIndex, svg)

drawTileWithConnectors :: Tile -> Render ()
drawTileWithConnectors tile = drawTile tile >> drawConnectors tile

drawTile :: Tile -> Render ()
drawTile Tile{..} = restoreStateAfter $ do
    let color = case tileType of
            Thin -> 0
            Thick -> 1
    newPath
    moveToVec tileP0
    lineToVec tileP1
    lineToVec tileP2
    clipPreserve
    mmaColor color 1
    strokePreserve
    mmaColor color 0.5
    fill

drawConnectors :: Tile -> Render ()
drawConnectors tile@Tile{..} = restoreStateAfter $ do
    let Distance unitLength = norm (tileP0 -. tileP1)
        r1 = Distance (0.3 * unitLength)
        r2 = Distance (0.2 * unitLength)
        r3 = Distance (0.8 * unitLength)
    polygonSketch (asPolygon tile)
    clip
    case tileType of
        Thin -> do
            circleSketch tileP0 r1
            stroke
            circleSketch tileP2 r2
            stroke
        Thick -> do
            circleSketch tileP0 r3
            stroke
            circleSketch tileP2 r1
            stroke

drawInscribedPentagons :: Tile -> Render ()
drawInscribedPentagons tile = restoreStateAfter $ do
    drawTile tile
    for_ (inscribedPentagons tile) $ \polygon -> do
        polygonSketchOpen polygon
        stroke

polygonSketchOpen :: Polygon -> Render ()
polygonSketchOpen (Polygon []) = pure ()
polygonSketchOpen (Polygon (Vec2 x y : vecs)) = do
    moveTo x y
    for_ vecs (\(Vec2 x' y') -> lineTo x' y')
