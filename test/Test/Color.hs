module Test.Color (tests) where

import Data.Colour.Names
import Data.Foldable (for_)
import Data.List (transpose)
import qualified Graphics.Rendering.Cairo as Cairo
import Test.Tasty
import Test.Tasty.HUnit

import Draw
import Test.Common (renderAllFormats)

tests :: TestTree
tests = testGroup "Color operations"
    [ testBrightness
    , testValue
    , testHue
    , testSaturation
    , testBlending
    ]

testBrightness :: TestTree
testBrightness = testCase "brightness" $ renderColorTable "docs/colors/brightness" $
    colorTable baseColors (\color brightness -> adjustBrightness (const brightness) color)

testValue :: TestTree
testValue = testCase "value" $ renderColorTable "docs/colors/value" $
    colorTable baseColors (\color value -> adjustValue (const value) color)

testHue :: TestTree
testHue = testCase "hue" $ renderColorTable "docs/colors/hue" $
    colorTable baseColors (\color hue -> adjustHue (+ 60*hue) color)

testSaturation :: TestTree
testSaturation = testCase "saturation" $ renderColorTable "docs/colors/saturation" $
    colorTable baseColors (\color saturation -> adjustSaturation (* saturation) color)

testBlending :: TestTree
testBlending = testCase "value" $ do
    renderColorTable "docs/colors/blending_white" $
        colorTable baseColors (\color factor -> blend factor white color)
    renderColorTable "docs/colors/blending_black" $
        colorTable baseColors (\color factor -> blend factor black color)
    renderColorTable "docs/colors/blending_blue"  $
        colorTable baseColors (\color factor -> blend factor blue  color)


baseColors :: [Color Double]
baseColors = [grey, maroon, olive, green, teal, navy, purple]

colorTable :: [Color Double] -> (Color Double -> Double -> Color Double) -> [[Color Double]]
colorTable colors generator =
    [ generator color <$> [0, 0.1 .. 1]
    | color <- colors ]

renderColorTable :: FilePath -> [[Color Double]] -> IO ()
renderColorTable file table = renderAllFormats width height file $ do
    Cairo.setLineWidth 0.2
    Cairo.translate 0 2
    for_ table $ \row -> do
        cairoScope $ do
            Cairo.translate 2 0
            for_ row $ \cell -> do
                Cairo.rectangle 0 0 16 16
                setColor cell
                Cairo.fillPreserve
                setColor black
                Cairo.stroke
                Cairo.translate 20 0
        Cairo.translate 0 20
  where
    width = 20 * length (transpose table)
    height = 20 * length table
