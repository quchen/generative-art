
{-# LANGUAGE RecordWildCards #-}
module Test.Penrose (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo hiding (transform, x, y)

import Draw
import Geometry
import Penrose

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit
import Data.Vector.Unboxed.Base (Vector(V_Any))

tests :: TestTree
tests = testGroup "Penrose tiling"
    [ testDecagonRose
    , testSubdivision
    , testInscribedPentagons
    ]

testDecagonRose :: TestTree
testDecagonRose = testCase "Decagon rose" test
  where
    test = renderAllFormats 200 200 "test/out/penrose/1_decagon_rose" $
        for_ (decagonRose (Vec2 100 100) 100) $ \face -> drawFace face >> drawConnectors face

testSubdivision :: TestTree
testSubdivision = testCase "Subdividing base rhombs" test
  where
    fitToBox = transform (translate' 0 60) . transform (scale' 100 100)
    baseRhombThick = fitToBox thickFaceBase
    baseRhombThin = transform (translate' 200 0) $ fitToBox thinFaceBase
    gen0 = baseRhombThick ++ baseRhombThin
    gen1 = subdivide =<< gen0
    gen2 = subdivide =<< gen1
    test = renderAllFormats 420 420 "test/out/penrose/2_subdivision" $ do
        translate 10 10
        for_ gen0 drawFace
        translate 0 140
        for_ gen1 drawFace
        translate 0 140
        for_ gen2 drawFace

testInscribedPentagons :: TestTree
testInscribedPentagons = testCase "Switch to pentagons & stars" test
  where
    test = renderAllFormats 200 200 "test/out/penrose/3_inscribed_pentagons" $
        for_ (inscribedPentagons =<< decagonRose (Vec2 100 100) 100) $ \polygon ->
            restoreStateAfter $ do
                polygonSketchOpen polygon
                stroke

drawFace :: Face -> Render ()
drawFace Face{..} = restoreStateAfter $ do
    let color = case faceType of
            Thin -> 0
            Thick -> 1
    newPath
    moveToVec faceP0
    lineToVec faceP1
    lineToVec faceP2
    clipPreserve
    mmaColor color 1
    strokePreserve
    mmaColor color 0.5
    fill

drawConnectors :: Face -> Render ()
drawConnectors face@Face{..} = restoreStateAfter $ do
    let Distance length = norm (faceP0 -. faceP1)
        r1 = Distance (0.3 * length)
        r2 = Distance (0.2 * length)
        r3 = Distance (0.8 * length)
    polygonSketch (asPolygon face)
    clip
    case faceType of
        Thin -> do
            circleSketch faceP0 r1
            stroke
            circleSketch faceP2 r2
            stroke
        Thick -> do
            circleSketch faceP0 r3
            stroke
            circleSketch faceP2 r1
            stroke

polygonSketchOpen :: Polygon -> Render ()
polygonSketchOpen (Polygon []) = pure ()
polygonSketchOpen (Polygon (Vec2 x y : vecs)) = do
    moveTo x y
    for_ vecs (\(Vec2 x' y') -> lineTo x' y')
