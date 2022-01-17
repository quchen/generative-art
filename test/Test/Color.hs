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
testBrightness = testCase "lightness" $ renderColorTable "docs/colors/lightness" $
    colorTable baseColors (\lightness -> adjustHsl id id (const lightness))

testValue :: TestTree
testValue = testCase "brightness" $ renderColorTable "docs/colors/brightness" $
    colorTable baseColors (\value -> adjustHsv id id (const value))

testHue :: TestTree
testHue = testCase "hue" $ renderColorTable "docs/colors/hue" $
    colorTable baseColors (\hue -> adjustHsv (+ 60*hue) id id)

testSaturation :: TestTree
testSaturation = testCase "saturation" $ do
    renderColorTable "docs/colors/saturation_hsv" $
        colorTable (drop 1 baseColors) (\saturation -> adjustHsv id (const saturation) id)
    renderColorTable "docs/colors/saturation_hsl" $
        colorTable (drop 1 baseColors) (\saturation -> adjustHsl id (const saturation) id)

testBlending :: TestTree
testBlending = testCase "value" $ do
    renderColorTable "docs/colors/blending_white" $
        colorTable baseColors (\factor -> blend factor white)
    renderColorTable "docs/colors/blending_black" $
        colorTable baseColors (\factor -> blend factor black)
    renderColorTable "docs/colors/blending_blue"  $
        colorTable baseColors (\factor -> blend factor blue)


baseColors :: [Color Double]
baseColors = [grey, maroon, olive, green, teal, navy, purple]

colorTable :: [Color Double] -> (Double -> Color Double -> Color Double) -> [[Color Double]]
colorTable colors generator =
    [ flip generator color <$> [0, 0.1 .. 1]
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
