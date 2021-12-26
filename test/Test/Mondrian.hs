module Test.Mondrian (tests) where

import Data.Foldable
import Graphics.Rendering.Cairo as Cairo hiding (transform, x, y)

import Draw
import Geometry
import Mondrian

import Test.Common
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Mondrian"
    [ explorativeTest
    ]

explorativeTest :: TestTree
explorativeTest = testCase "Exploration" test
  where
    test = renderAllFormats 1000 1000 "mondrianTest" $ do
        let testMondrian = removeEdge Down (2, 3) $ removeEdge Down (2, 2) $ mondrianBaseGrid 10 10
        for_ (zip (asPolygons 100 testMondrian) [0..]) $ \(poly, color) -> do
            polygonSketch poly
            mondrianColor color
            fillPreserve
            setSourceRGB 0 0 0
            setLineWidth 20
            stroke

mondrianColor :: Int -> Render ()
mondrianColor color = case color of
    0 -> white
    1 -> white
    2 -> white
    3 -> white
    4 -> blue
    5 -> white
    6 -> red
    7 -> white
    8 -> white
    9 -> yellow
    10 -> blue
    11 -> red
    12 -> white
    13 -> blue
    14 -> yellow
    15 -> red
    n -> mondrianColor (n `mod` 16)
  where
    white = setSourceRGB 255 255 255
    red = setSourceRGB 255 0 0
    blue = setSourceRGB 0 0 255
    yellow = setSourceRGB 255 255 0
