module Test.Geometry.Processes.ApollonianGasket (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as C

import Draw
import Geometry as G
import Geometry.Processes.ApollonianGasket

import Test.TastyAll



tests :: TestTree
tests = testGroup "Apollonian gasket"
    [ testGroup "Pretty bugs :-D"
        [ forgettingGen0
        , missingTheMinus
        ]
    , correctGasket
    ]

forgettingGen0 :: TestTree
forgettingGen0 = testCase "Forgetting to use the gen0 circles" $ renderAllFormats 300 300 "out/buggy_but_pretty_1" $ do
    let gen0L = toApoCircle $ Circle (Vec2 100 100) 50
        gen0R = toApoCircle $ Circle (Vec2 200 100) 50
        gen0B = toApoCircle $ Circle (G.transform (rotateAround (Vec2 100 100) (deg 60)) (Vec2 200 100)) 50

        large = newCircle (-) (-) gen0L gen0R gen0B

        gen1T = newCircle (+) (+) large gen0L gen0R
        gen1L = newCircle (+) (-) large gen0L gen0B
        gen1R = newCircle (+) (+) large gen0R gen0B

        recurse [] = []
        recurse ((c1, c2, c3) : rest) =
            let new@(ApoCircle _ k) = newCircle (+) (+) c1 c2 c3
            in if k > 10
                then recurse rest
                else new : recurse ((c1, c2, new) : (c1, c3, new) : (c2, c3, new) : rest)


        apoCircleSketch c =
            let Circle center radius = toCircle c
            in circleSketch center (abs radius)

        apoCircles = concat
            [ []
            -- , [large]
            -- , [gen0L, gen0R, gen0B]
            , [gen1T, gen1L, gen1R]
            , recurse [(large, gen1T, gen1L)]
            , recurse [(large, gen1T, gen1R)]
            , recurse [(gen1T, gen1L, gen1R)]
            , recurse [(large, gen1L, gen1R)]
            ]

    for_ (zip [0..] apoCircles) $ \(i, apo) -> cairoScope $ do
        setLineWidth 1
        setColor (rocket (1-fromIntegral i/fromIntegral (length apoCircles)))
        apoCircleSketch apo
        stroke

missingTheMinus :: TestTree
missingTheMinus = testCase "Missing that one minus" $ renderAllFormats 300 300 "out/buggy_but_pretty_2" $ do
    let gen0L = toApoCircle $ Circle (Vec2 100 100) 50
        gen0R = toApoCircle $ Circle (Vec2 200 100) 50
        gen0B = toApoCircle $ Circle (G.transform (rotateAround (Vec2 100 100) (deg 60)) (Vec2 200 100)) 50

        large = newCircle (-) (-) gen0L gen0R gen0B

        recurse [] = []
        recurse ((c1, c2, c3) : rest) =
            let new@(ApoCircle _ k) = newCircle (+) (+) c1 c2 c3
                -- the bottom-left circle of generation 1 needs (+) (-), otherwise that part
                -- folds upwards-looking. I have no idea why.
            in if k > 1
                then recurse rest
                else new : recurse ((c1, c2, new) : (c1, c3, new) : (c2, c3, new) : rest)


        apoCircleSketch c =
            let Circle center radius = toCircle c
            in circleSketch center (abs radius)

        apoCircles = concat
            [ []
            , [large]
            , [gen0B, gen0L, gen0R]
            , recurse [(gen0B, gen0L, gen0R)]
            , recurse [(large, gen0L, gen0R)]
            , recurse [(large, gen0B, gen0R)]
            , recurse [(large, gen0B, gen0L)]
            ]

    for_ (zip [0..] apoCircles) $ \(i, apo) -> cairoScope $ do
        setLineWidth 1
        setColor (rocket (1-fromIntegral i/fromIntegral (length apoCircles)))
        apoCircleSketch apo
        stroke


correctGasket :: TestTree
correctGasket = testCase "The Gasket" $ renderAllFormats 300 300 "out/correct" $ do
    let gen0L = toApoCircle $ Circle (Vec2 100 100) 50
        gen0R = toApoCircle $ Circle (Vec2 200 100) 50
        gen0B = toApoCircle $ Circle (G.transform (rotateAround (Vec2 100 100) (deg 60)) (Vec2 200 100)) 50

        large = newCircle (-) (-) gen0L gen0R gen0B

        gen1T = newCircle (+) (+) large gen0L gen0R
        gen1L = newCircle (+) (-) large gen0L gen0B
        gen1R = newCircle (+) (+) large gen0R gen0B

        recurse [] = []
        recurse ((c1, c2, c3) : rest) =
            let new@(ApoCircle _ k) = newCircle (+) (+) c1 c2 c3
            in if k > 2
                then recurse rest
                else new : recurse ((c1, c2, new) : (c1, c3, new) : (c2, c3, new) : rest)


        apoCircleSketch c =
            let Circle center radius = toCircle c
            in circleSketch center (abs radius)

        apoCircles = concat
            [ []
            , [large]
            , [gen0B, gen0L, gen0R]
            , recurse [(gen0L, gen0R, gen0B)]
            , gen1T : recurse [(gen1T, gen0L, large)]
            , gen1T : recurse [(gen1T, gen0R, large)]
            , gen1R : recurse [(gen1R, gen0R, large)]
            , gen1R : recurse [(gen1R, gen0B, large)]
            , gen1L : recurse [(gen1L, gen0B, large)]
            , gen1L : recurse [(gen1L, gen0L, large)]
            ]

    for_ (zip [0..] apoCircles) $ \(i, apo) -> cairoScope $ do
        setLineWidth 1
        setColor (rocket (1-fromIntegral i/fromIntegral (length apoCircles)))
        apoCircleSketch apo
        stroke
