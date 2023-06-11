{-# LANGUAGE OverloadedStrings #-}

module Test.Draw.Text (tests) where



import qualified Graphics.Rendering.Cairo as C
import           Test.TastyAll

import Draw
import Geometry as G



tests :: TestTree
tests = testGroup "Plotting text"
    [ testGroup "Text alignment" (testAlignment <$> [ (h, v) | h <- [HLeft, HCenter, HRight], v <- [VBottom, VCenter, VTop] ])
    , testSize
    ]

testAlignment :: (HAlign, VAlign) -> TestTree
testAlignment alignment@(hAlign, vAlign) = testVisual (show alignment) 100 100 file $ \(w, h) -> do
    coordinateSystem (MathStandard_ZeroCenter_XRight_YUp w h)
    cairoScope $ do
        setColor (mma 1)
        sketch (Line (Vec2 0 (-100)) (Vec2 0 100))
        sketch (Line (Vec2 (-100) 0) (Vec2 100 0))
        C.stroke
    cairoScope $ do
        setColor (mma 0)
        for_ (plotText def {_textHAlign = hAlign, _textVAlign = vAlign} "align") sketch
        C.stroke
  where
    file = "docs/font/alignment_" ++ show hAlign ++ "_" ++ show vAlign

testSize :: TestTree
testSize = testVisual "Font size" 580 200 "docs/font/size" $ \(w, h) -> do
    coordinateSystem (MathStandard_ZeroBottomLeft_XRight_YUp h)
    for_ [10, 20, 30, 50] $ \size -> do
        C.translate 0 10
        cairoScope $ do
            setColor (mma 1)
            sketch (Line (Vec2 2 0) (Vec2 2 size))
            C.stroke
            C.setDash [4, 4] 0
            sketch (Line (Vec2 2 0) (Vec2 w 0))
            sketch (Line (Vec2 2 size) (Vec2 w size))
            C.stroke
        cairoScope $ do
            setColor (mma 0)
            for_ (plotText def {_textHeight = size, _textStartingPoint = Vec2 10 0} "Test xX fF gG") sketch
            C.stroke
        C.translate 0 (size + 10)
