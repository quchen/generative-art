{-# LANGUAGE OverloadedStrings #-}

module Test.Draw.Text (tests) where



import qualified Graphics.Rendering.Cairo as C
import Test.TastyAll

import Draw
import Draw.Text
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
        setColor (mathematica97 1)
        sketch (Line (Vec2 0 (-100)) (Vec2 0 100))
        sketch (Line (Vec2 (-100) 0) (Vec2 100 0))
        C.stroke
    cairoScope $ do
        setColor (mathematica97 0)
        for_ (plotText def {_textHAlign = hAlign, _textVAlign = vAlign} "align") sketch
        C.stroke
  where
    file = "docs/font/alignment_" ++ show hAlign ++ "_" ++ show vAlign

testSize :: TestTree
testSize = testCase "Text size" $ pure ()
