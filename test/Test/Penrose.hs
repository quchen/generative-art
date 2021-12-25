
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

tests :: TestTree
tests = testGroup "Penrose tiling"
    [ testDecagonRose
    , testSubdivision
    ]

testDecagonRose :: TestTree
testDecagonRose = testCase "Decagon rose" test
  where
    test = renderAllFormats 200 200 "test/out/penrose/1_decagon_rose" $
        for_ (decagonRose (Vec2 100 100) 100) drawFace

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
