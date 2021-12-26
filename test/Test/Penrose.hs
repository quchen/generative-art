
{-# LANGUAGE RecordWildCards #-}
module Test.Penrose (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)

import Draw
import Geometry
import Penrose

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Penrose tiling"
    [ testBaseConfigurations
    , testSubdivision
    , testInscribedPentagons
    ]

testBaseConfigurations :: TestTree
testBaseConfigurations = testCase "Base configurations" test
  where
    test = renderAllFormats 860 200 "test/out/penrose/1_base_configurations" $ do
        for_ (star1 (Vec2 100 100) 100) $ \tile -> drawTile tile >> drawConnectors tile
        Cairo.translate 220 0
        for_ (star2 (Vec2 100 100) 100) $ \tile -> drawTile tile >> drawConnectors tile
        Cairo.translate 220 0
        for_ (decagonRose (Vec2 100 100) 100) $ \tile -> drawTile tile >> drawConnectors tile
        Cairo.translate 220 0
        for_ (asymmetricDecagon (Vec2 100 100) 100) $ \tile -> drawTile tile >> drawConnectors tile

testSubdivision :: TestTree
testSubdivision = testCase "Subdividing base rhombs" test
  where
    fitToBox = transform (translate' (Vec2 0 60) <> scale' 100 100)
    baseRhombThick = fitToBox thickTileBase
    baseRhombThin = transform (translate' (Vec2 200 0)) $ fitToBox thinTileBase
    gen0 = baseRhombThick ++ baseRhombThin
    gen1 = subdivide =<< gen0
    gen2 = subdivide =<< gen1
    test = renderAllFormats 420 420 "test/out/penrose/2_subdivision" $ do
        Cairo.translate 10 10
        for_ gen0 drawTile
        Cairo.translate 0 140
        for_ gen1 drawTile
        Cairo.translate 0 140
        for_ gen2 drawTile

testInscribedPentagons :: TestTree
testInscribedPentagons = testCase "Switch to pentagons & stars" test
  where
    test = renderAllFormats 200 200 "test/out/penrose/3_inscribed_pentagons" $
        for_ (inscribedPentagons =<< decagonRose (Vec2 100 100) 100) $ \polygon ->
            restoreStateAfter $ do
                polygonSketchOpen polygon
                stroke

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
    let Distance length = norm (tileP0 -. tileP1)
        r1 = Distance (0.3 * length)
        r2 = Distance (0.2 * length)
        r3 = Distance (0.8 * length)
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

polygonSketchOpen :: Polygon -> Render ()
polygonSketchOpen (Polygon []) = pure ()
polygonSketchOpen (Polygon (Vec2 x y : vecs)) = do
    moveTo x y
    for_ vecs (\(Vec2 x' y') -> lineTo x' y')
