{-# LANGUAGE RecordWildCards #-}
module Test.Geometry.Processes.Penrose (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as C hiding (x, y)

import Draw
import Geometry as G
import Geometry.Processes.Penrose

import Test.TastyAll



tests :: TestTree
tests = testGroup "Penrose tiling"
    [ testBaseConfigurations
    , testSubdivision
    , testInscribedPentagons
    , testSubdivisionWithInscribedPentagons
    ]

testBaseConfigurations :: TestTree
testBaseConfigurations = testCase "Base configurations" test
  where
    test = renderAllFormats 860 200 "docs/penrose/1_base_configurations" $ do
        for_ (star1 (Vec2 100 100) 100) drawTileWithConnectors
        C.translate 220 0
        for_ (star2 (Vec2 100 100) 100) drawTileWithConnectors
        C.translate 220 0
        for_ (decagonRose (Vec2 100 100) 100) drawTileWithConnectors
        C.translate 220 0
        for_ (asymmetricDecagon (Vec2 100 100) 100) drawTileWithConnectors

testSubdivision :: TestTree
testSubdivision = testCase "Subdividing base rhombs" test
  where
    fitToBox = G.scale 100
    baseRhombThick = G.transform fitToBox thickTileBase
    baseRhombThin = G.transform (G.translate (Vec2 0 120) <> fitToBox) thinTileBase
    gen0 = baseRhombThick ++ baseRhombThin
    gen1 = subdivide =<< gen0
    gen2 = subdivide =<< gen1
    test = renderAllFormats 600 200 "docs/penrose/2_subdivision" $ do
        C.translate 10 10
        for_ gen0 drawTileWithConnectors
        C.translate 150 0
        drawArrows
        C.translate 50 0
        for_ gen1 drawTileWithConnectors
        C.translate 150 0
        drawArrows
        C.translate 50 0
        for_ gen2 drawTileWithConnectors
    drawArrows = do
        arrowSketch (Line (Vec2 0 50) (Vec2 30 50)) def >> stroke
        arrowSketch (Line (Vec2 20 150) (Vec2 50 150)) def >> stroke

testInscribedPentagons :: TestTree
testInscribedPentagons = testCase "Switch to pentagons & stars" test
  where
    test = renderAllFormats 200 200 "docs/penrose/3_inscribed_pentagons" $
        for_ (decagonRose (Vec2 100 100) 100) drawInscribedPentagons

testSubdivisionWithInscribedPentagons :: TestTree
testSubdivisionWithInscribedPentagons = testCase "Subdivision rules with pentagons & stars" test
  where
    fitToBox = G.scale 100
    baseRhombThick = G.transform fitToBox thickTileBase
    baseRhombThin = G.transform (G.translate (Vec2 0 120) <> fitToBox) thinTileBase
    gen0 = baseRhombThick ++ baseRhombThin
    gen1 = subdivide =<< gen0
    gen2 = subdivide =<< gen1
    test = renderAllFormats 600 200 "docs/penrose/4_subdivision_with_pentagons" $ do
        C.translate 10 10
        for_ gen0 drawInscribedPentagons
        C.translate 150 0
        drawArrows
        C.translate 50 0
        for_ gen1 drawInscribedPentagons
        C.translate 150 0
        drawArrows
        C.translate 50 0
        for_ gen2 drawInscribedPentagons
    drawArrows = do
        arrowSketch (Line (Vec2 0 50) (Vec2 30 50)) def >> stroke
        arrowSketch (Line (Vec2 20 150) (Vec2 50 150)) def >> stroke

drawTileWithConnectors :: Tile -> Render ()
drawTileWithConnectors tile = drawTile tile >> drawConnectors tile

drawTile :: Tile -> Render ()
drawTile Tile{..} = cairoScope $ do
    let color = case tileType of
            Thin -> 0
            Thick -> 1
    newPath
    moveToVec tileP0
    lineToVec tileP1
    lineToVec tileP2
    clipPreserve
    setColor $ mathematica97 color
    strokePreserve
    setColor $ mathematica97 color `withOpacity` 0.5
    fill

drawConnectors :: Tile -> Render ()
drawConnectors tile@Tile{..} = cairoScope $ do
    let unitLength = norm (tileP0 -. tileP1)
        r1 = 0.3 * unitLength
        r2 = 0.2 * unitLength
        r3 = 0.8 * unitLength
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
drawInscribedPentagons tile = cairoScope $ do
    drawTile tile
    for_ (inscribedPentagons tile) $ \polygon -> do
        polygonSketchOpen polygon
        stroke

polygonSketchOpen :: Polygon -> Render ()
polygonSketchOpen (Polygon []) = pure ()
polygonSketchOpen (Polygon (Vec2 x y : vecs)) = do
    moveTo x y
    for_ vecs (\(Vec2 x' y') -> lineTo x' y')
