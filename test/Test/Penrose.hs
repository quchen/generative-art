
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
    baseRhomb = thickFaceBase (Vec2 0 50) 100
    subdividedRhomb = subdivide =<<  baseRhomb
    test = renderAllFormats 240 120 "test/out/penrose/2_subdivision" $ do
        translate 10 10
        for_ baseRhomb drawFace
        translate 120 0
        for_ subdividedRhomb drawFace


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
