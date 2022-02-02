module Test.Draw.Color (tests) where



import Data.Colour.Names
import Data.List                (transpose)
import Graphics.Rendering.Cairo as C
import Test.Tasty
import Test.Tasty.HUnit

import Draw
import Numerics.Interpolation
import Test.Common            (renderAllFormats)



tests :: TestTree
tests = testGroup "Colors"
    [ testGroup "Operations"
        [ testBrightness
        , testValue
        , testHue
        , testSaturation
        , testBlending
        ]
    , testGroup "Schemes"
        [ testGroup "Discrete"
            [ testCase "Mathematica ColorData[97]" $ renderDiscrete "docs/colors/schemes/mathematica_ColorData97" mathematica97 10
            ]
        , testGroup "Continuous"
            [ testGroup "Visually uniform"
                [ testCase "inferno" $ renderContinuous "docs/colors/schemes/inferno"         inferno         (0,1)
                , testCase "plasma"  $ renderContinuous "docs/colors/schemes/plasma"          plasma          (0,1)
                , testCase "viridis" $ renderContinuous "docs/colors/schemes/viridis"         viridis         (0,1)
                , testCase "cividis" $ renderContinuous "docs/colors/schemes/cividis"         cividis         (0,1)
                ]
            , testCase "turbo"           $ renderContinuous "docs/colors/schemes/turbo"           turbo           (0,1)
            , testCase "twilight"        $ renderContinuous "docs/colors/schemes/twilight"        twilight        (0,2)
            , testCase "twilightShifted" $ renderContinuous "docs/colors/schemes/twilightShifted" twilightShifted (0,2)
            ]
        ]
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
    C.setLineWidth 0.2
    C.translate 0 2
    for_ table $ \row -> do
        cairoScope $ do
            C.translate 2 0
            for_ row $ \cell -> do
                C.rectangle 0 0 16 16
                setColor cell
                C.fillPreserve
                setColor black
                C.stroke
                C.translate 20 0
        C.translate 0 20
  where
    width = 20 * length (transpose table)
    height = 20 * length table

renderContinuous :: FilePath -> (Double -> Color Double) -> (Double, Double) -> IO ()
renderContinuous file colorF (lo,hi) = renderAllFormats width height file $ do
    for_ [-10..width+10] $ \x -> do
        C.rectangle (fromIntegral x) 0 (fromIntegral x+1) (fromIntegral height-1)
        setColor (colorF (linearInterpolate (0,fromIntegral width-1) (lo,hi) (fromIntegral x)))
        fill
  where
    width = 300
    height = 32

renderDiscrete :: FilePath -> (Int -> Color Double) -> Int -> IO ()
renderDiscrete file colorF n = renderAllFormats (width*n) height file $ do
    for_ [0 .. n] $ \i -> do
        C.rectangle (fromIntegral i*width) 0 (fromIntegral (i+1)*width) (height-1)
        setColor (colorF i)
        fill
  where
    width, height :: Num a => a
    width = 32
    height = 32
