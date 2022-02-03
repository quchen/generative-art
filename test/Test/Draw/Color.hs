module Test.Draw.Color (tests) where



import           Control.Monad
import           Data.Colour.Names
import           Data.List                (transpose)
import qualified Data.Vector              as V
import           Graphics.Rendering.Cairo as C
import           Test.Tasty
import           Test.Tasty.HUnit

import Draw
import Geometry as G
import Draw.Color.Schemes.Internal.Common
import Numerics.Interpolation

import Test.Common
import Test.Helpers



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
            [ testGroup "Mathematica"
                [ testGroup "ColorData[97]"
                    [ testCase "Visual" $ renderDiscrete "docs/colors/schemes/discrete/mathematica/ColorData97" mathematica97 16
                    , testCase "Check against Mathematica output" checkMma97
                    ]
                , testGroup "Color Brewer 2"
                    [ testCase "Set2"    $ renderDiscrete "docs/colors/schemes/discrete/colorbrewer2/set2"    set2    16
                    , testCase "Accent"  $ renderDiscrete "docs/colors/schemes/discrete/colorbrewer2/accent"  accent  16
                    , testCase "Set1"    $ renderDiscrete "docs/colors/schemes/discrete/colorbrewer2/set1"    set1    16
                    , testCase "Set3"    $ renderDiscrete "docs/colors/schemes/discrete/colorbrewer2/set3"    set3    16
                    , testCase "Dark2"   $ renderDiscrete "docs/colors/schemes/discrete/colorbrewer2/dark2"   dark2   16
                    , testCase "Paired"  $ renderDiscrete "docs/colors/schemes/discrete/colorbrewer2/paired"  paired  16
                    , testCase "Pastel2" $ renderDiscrete "docs/colors/schemes/discrete/colorbrewer2/pastel2" pastel2 16
                    , testCase "Pastel1" $ renderDiscrete "docs/colors/schemes/discrete/colorbrewer2/pastel1" pastel1 16
                    ]
                ]
            ]
        , testGroup "Continuous"
            [ testGroup "Matplotlib"
                [ testGroup "Visually uniform"
                    [ testCase "cividis" $ renderContinuous "docs/colors/schemes/continuous/matplotlib/cividis" cividis (0,1)
                    , testCase "inferno" $ renderContinuous "docs/colors/schemes/continuous/matplotlib/inferno" inferno (0,1)
                    , testCase "magma"   $ renderContinuous "docs/colors/schemes/continuous/matplotlib/magma"   magma   (0,1)
                    , testCase "plasma"  $ renderContinuous "docs/colors/schemes/continuous/matplotlib/plasma"  plasma  (0,1)
                    , testCase "viridis" $ renderContinuous "docs/colors/schemes/continuous/matplotlib/viridis" viridis (0,1)
                    ]
                , testGroup "Other"
                    [ testCase "turbo"           $ renderContinuous "docs/colors/schemes/continuous/matplotlib/turbo"           turbo           (0,1)
                    , testCase "twilight"        $ renderContinuous "docs/colors/schemes/continuous/matplotlib/twilight"        twilight        (0,1)
                    , testCase "twilightShifted" $ renderContinuous "docs/colors/schemes/continuous/matplotlib/twilightShifted" twilightShifted (0,1)
                    ]
                ]
            , testGroup "Color Brewer 2"
                [ testGroup "Sequential"
                    [ testCase "OrRd"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/orRd"    orRd    (0,1)
                    , testCase "PuBu"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/puBu"    puBu    (0,1)
                    , testCase "BuPu"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/buPu"    buPu    (0,1)
                    , testCase "Oranges" $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/oranges" oranges (0,1)
                    , testCase "BuGn"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/buGn"    buGn    (0,1)
                    , testCase "YlOrBr"  $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/ylOrBr"  ylOrBr  (0,1)
                    , testCase "YlGn"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/ylGn"    ylGn    (0,1)
                    , testCase "Reds"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/reds"    reds    (0,1)
                    , testCase "RdPu"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/rdPu"    rdPu    (0,1)
                    , testCase "Greens"  $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/greens"  greens  (0,1)
                    , testCase "YlGnBu"  $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/ylGnBu"  ylGnBu  (0,1)
                    , testCase "Purples" $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/purples" purples (0,1)
                    , testCase "GnBu"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/gnBu"    gnBu    (0,1)
                    , testCase "Greys"   $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/greys"   greys   (0,1)
                    , testCase "YlOrRd"  $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/ylOrRd"  ylOrRd  (0,1)
                    , testCase "PuRd"    $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/puRd"    puRd    (0,1)
                    , testCase "Blues"   $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/blues"   blues   (0,1)
                    , testCase "PuBuGn"  $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/puBuGn"  puBuGn  (0,1)
                    ]
                , testGroup "Divisive"
                    [ testCase "Spectral" $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/spectral" spectral (0,1)
                    , testCase "RdYlGn"   $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/rdYlGn"   rdYlGn   (0,1)
                    , testCase "RdBu"     $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/rdBu"     rdBu     (0,1)
                    , testCase "PiYG"     $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/piYG"     piYG     (0,1)
                    , testCase "PRGn"     $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/pRGn"     pRGn     (0,1)
                    , testCase "RdYlBu"   $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/rdYlBu"   rdYlBu   (0,1)
                    , testCase "BrBG"     $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/brBG"     brBG     (0,1)
                    , testCase "RdGy"     $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/rdGy"     rdGy     (0,1)
                    , testCase "PuOr"     $ renderContinuous "docs/colors/schemes/continuous/colorbrewer2/puOr"     puOr     (0,1)
                    ]
                ]
            ]
        ]
    ]

testBrightness :: TestTree
testBrightness = testCase "lightness" $ renderColorTable "docs/colors/operations/lightness" $
    colorTable baseColors (\lightness -> adjustHsl id id (const lightness))

testValue :: TestTree
testValue = testCase "brightness" $ renderColorTable "docs/colors/operations/brightness" $
    colorTable baseColors (\value -> adjustHsv id id (const value))

testHue :: TestTree
testHue = testCase "hue" $ renderColorTable "docs/colors/operations/hue" $
    colorTable baseColors (\hue -> adjustHsv (+ 60*hue) id id)

testSaturation :: TestTree
testSaturation = testCase "saturation" $ do
    renderColorTable "docs/colors/operations/saturation_hsv" $
        colorTable (drop 1 baseColors) (\saturation -> adjustHsv id (const saturation) id)
    renderColorTable "docs/colors/operations/saturation_hsl" $
        colorTable (drop 1 baseColors) (\saturation -> adjustHsl id (const saturation) id)

testBlending :: TestTree
testBlending = testCase "value" $ do
    renderColorTable "docs/colors/operations/blending_white" $
        colorTable baseColors (\factor -> blend factor white)
    renderColorTable "docs/colors/operations/blending_black" $
        colorTable baseColors (\factor -> blend factor black)
    renderColorTable "docs/colors/operations/blending_blue"  $
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
        C.rectangle (fromIntegral x) 0 (fromIntegral x+1) height
        setColor (colorF (linearInterpolate (0,width-1) (lo,hi) (fromIntegral x)))
        fill
  where
    width, height :: Num a => a
    width = 480
    height = 32

-- | Render discrete swatches until a color repeats, or until a maximum number is reached.
renderDiscrete
    :: FilePath
    -> (Int -> Color Double)
    -> Int -- ^ Maximum number of swatches
    -> IO ()
renderDiscrete file colorF maxSwatches = do
    let allColors = colorF 0 : takeWhile (/= colorF 0) (map colorF [1..])
        colors = take maxSwatches allColors
        numColors = length colors
    renderAllFormats (width*numColors) height file $ do
        for_ (zip [0..] colors) $ \(i, color) -> cairoScope $ do
            C.rectangle (fromIntegral i*width) (-1) (fromIntegral (i+1)*width) height
            setColor color
            fill

        -- »dot dot dot« when cutting off early
        when (allColors `isLongerThan` colors) $ cairoScope $ do
            setColor black
            setLineWidth 1
            let lastCellCenter = Vec2 (fromIntegral numColors*width - width/2) (height/2)
            arrowSketch (angledLine (lastCellCenter +. Vec2 2 0) (deg 0) 10)
                def {arrowheadSize = 5 }
            stroke
            for_ [-10, -6, -2] $ \offset -> do
                circleSketch (lastCellCenter +. Vec2 offset 0) 1
                fill
  where
    width, height :: Num a => a
    width = 32
    height = 32

    isLongerThan (_:xs) (_:ys) = isLongerThan xs ys
    isLongerThan []     []     = False
    isLongerThan (_:_)  []     = True
    isLongerThan []     (_:_)  = True

checkMma97 :: Assertion
checkMma97 = sequence_ $ flip V.imap mma97reference $ \i expected -> do
    let err = unlines
            [ "ColorData[97][" ++ show i ++ "] is " ++ show expected
            , "but our function yields " ++ show actual
            ]
        actual = mathematica97 i
    assertBool err (expected ~== actual)

mma97reference :: V.Vector (Color Double)
mma97reference = fmap toColor $ V.fromList
    [ RGB 0.368417            0.506779            0.709798
    , RGB 0.880722            0.611041            0.142051
    , RGB 0.560181            0.691569            0.194885
    , RGB 0.922526            0.385626            0.209179
    , RGB 0.528488            0.470624            0.701351
    , RGB 0.772079            0.431554            0.102387
    , RGB 0.363898            0.618501            0.782349
    , RGB 1.000000            0.75                0
    , RGB 0.647624            0.37816             0.614037
    , RGB 0.571589            0.586483            0
    , RGB 0.915               0.3325              0.2125
    , RGB 0.40082222609352647 0.5220066643438841  0.85
    , RGB 0.9728288904374106  0.621644452187053   0.07336199581899142
    , RGB 0.736782672705901   0.358               0.5030266573755369
    , RGB 0.28026441037696703 0.715               0.4292089322474965
    , RGB 0.838355547812947   0.44746667828057946 0.0208888695323676
    , RGB 0.5833680111493557  0.4126186601628758  0.8290799721266107
    , RGB 0.8996399512215667  0.7463488834690629  0
    , RGB 0.8439466852489265  0.3467106629502147  0.3309221912517893
    , RGB 0.28240003484173815 0.6090799721266095  0.7538800418100857
    , RGB 0.9324333565611593  0.5282889043741062  0.0921900209050434
    , RGB 0.6753413537738198  0.3589675436319385  0.5991466155654507
    , RGB 0.5407932311309059  0.715               0.09762679674248334
    , RGB 0.8857244243136628  0.3764133635295058  0.1393110607841571
    , RGB 0.47401116530937026 0.47809330081437784 0.85
    , RGB 0.9874666782805795  0.6948333914028977  0.033839968642435214
    , RGB 0.7748409210981391  0.358               0.4444755060028629
    , RGB 0.16397784358994957 0.7038177251280403  0.6117734123079395
    , RGB 0.8613800418100862  0.48092002787339083 0.02824203762907758
    , RGB 0.6184987019729621  0.39212575718243875 0.7412532450675947
    , RGB 0.7678998606330495  0.7317110956258943  0
    , RGB 0.8878600487784333  0.33792799024431336 0.2577332520359445
    , RGB 0.35558897405758294 0.5505288207539337  0.8417067688690995
    , RGB 0.9637822400302223  0.5764112001511111  0.0977879519184
    , RGB 0.7132613816472081  0.358               0.5392132590042952
    , RGB 0.37977756485605163 0.715               0.3025558265468435
    , RGB 0.8564488486273256  0.42032672705901153 0.06612212156831418
    , RGB 0.5472001045252132  0.43417993728487203 0.85
    , RGB 0.9810598048862722  0.7553955338762525  0
    , RGB 0.8168067340273636  0.3521386531945273  0.3761554432877274
    , RGB 0.23716678280579248 0.645266573755366   0.699600139366951
    , RGB 0.9052934053395919  0.510195603559728   0.06776406480563275
    , RGB 0.6536293927965667  0.37163285420200276 0.6534265180085832
    , RGB 0.6361597700445392  0.7170733077827265  0
    , RGB 0.9038177251280404  0.3492734123079395  0.18454431282010084
    , RGB 0.428777913273419   0.5052332520359486  0.85
    , RGB 0.9784200278733908  0.649600139366954   0.058265924741844846
    , RGB 0.7513196300394465  0.358               0.48066210763162087
    , RGB 0.2187618985811806  0.715               0.5074848563512248
    , RGB 0.8342400905885153  0.4628267270590103  0.0038160815296638794
    , RGB 0.596786740995709   0.404791067752503   0.7955331475107271
    , RGB 0.8493197142977551  0.7407577460330839  0
    , RGB 0.8607200975568693  0.34335598048862614 0.30296650407188447
    , RGB 0.3103557220216354  0.5867154223826917  0.7874268664259625
    , RGB 0.9492067688690977  0.5394711792460651  0.10728609198218791
    , RGB 0.6897400905885174  0.358               0.5753998606330502
    , RGB 0.47929071933511125 0.715               0.1759027208462221
    , RGB 0.8745421494417032  0.39318677583744527 0.11135537360425793
    , RGB 0.5019668524892619  0.46131988850644284 0.85
    , RGB 0.9930578157165594  0.7227890785827968  0.01874389756528967
    , RGB 0.7896667828057927  0.3575666434388414  0.4213886953236787
    , RGB 0.19193353076984873 0.681453175384121   0.6453202369238185
    , RGB 0.8781534541180211  0.4921023027453475  0.04333810870621905
    , RGB 0.6319174318193065  0.38429816477207124 0.7077064204517338
    , RGB 0.7175796237092107  0.7261199581899123  0
    , RGB 0.9046334610863751  0.334573307782725   0.22977756485604156
    , RGB 0.38354466123747527 0.5323732032575149  0.85
    , RGB 0.969373377466199   0.6043668873309952  0.08269188084126262
    , RGB 0.7277983389807635  0.358               0.5168487092603637
    , RGB 0.31827505306025683 0.715               0.3808317506505822
    , RGB 0.845266573755366   0.437100139366951   0.03816643438841502
    , RGB 0.5750747800184488  0.4174563783225715  0.8498130499538777
    , RGB 0.9307395679624266  0.7498043964402695  0
    , RGB 0.8335801463353031  0.3487839707329394  0.3481997561078282
    , RGB 0.26512246998569167 0.6229020240114467  0.73314696398283
    , RGB 0.922066817647527   0.5213778784316846  0.08286013588277422
    , RGB 0.6670481226429111  0.3638052617916352  0.6198796933927223
    , RGB 0.5788038738141875  0.715               0.049249615145579635
    , RGB 0.8926354502560807  0.366046824615879   0.15658862564020168
    , RGB 0.4567336004533182  0.4884598397280091  0.85
    , RGB 0.9840111653093676  0.677555826546838   0.04316985366470745
    , RGB 0.7658565873730018  0.358               0.4582975578876895
    , RGB 0.15725938678540247 0.715               0.5857607804549424
    , RGB 0.8510135028964549  0.47400900193096995 0.018912152606809424
    , RGB 0.6102054708420535  0.39696347534213544 0.7619863228948663
    , RGB 0.7989994773739094  0.735166608597101   0
    , RGB 0.8774935098648088  0.34000129802703827 0.2750108168919853
    , RGB 0.3383114092015346  0.5643508726387724  0.8209736910418415
    , RGB 0.9603267270590103  0.5591336352950514  0.10711783694067224
    , RGB 0.7042770479220728  0.358               0.5530353108891188
    , RGB 0.4177882075393331  0.715               0.25417864494993975
    , RGB 0.8633598745697435  0.40996018814538476 0.08339968642435877
    , RGB 0.529922539669161   0.44454647619850335 0.85
    , RGB 0.9986489531525362  0.7507447657626809  0.0036478264881522893
    , RGB 0.8064401951137368  0.3542119609772526  0.39343300814377197
    , RGB 0.2198892179497479  0.6590886256402017  0.6788670615396974
    , RGB 0.8949268664259606  0.5032845776173072  0.05843417978336459
    , RGB 0.6453361616656581  0.37647057236169945 0.6741595958358548
    , RGB 0.6672593867853922  0.7205288207539324  0
    , RGB 0.9107287510704583  0.33890687339431275 0.20182187767614543
    ]
