
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
    ]

testDecagonRose :: TestTree
testDecagonRose = testCase "Decagon rose" test
  where
    test = renderAllFormats 200 200 "test/out/penrose/1_decagon_rose" $
        for_ (decagonRose (Vec2 100 100) 100) drawFace

drawFace :: Face -> Render ()
drawFace Face{..} = restoreStateAfter $ do
    let color = case faceType of
            Thin -> 0
            Thick -> 1
    newPath
    moveToVec faceP0
    lineToVec faceP1
    lineToVec faceP2
    mmaColor color 0.5
    fill
