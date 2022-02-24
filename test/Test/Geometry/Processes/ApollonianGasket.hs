module Test.Geometry.Processes.ApollonianGasket (tests) where



import Data.Foldable
import Graphics.Rendering.Cairo as C

import           Draw                                hiding (Circle)
import qualified Draw                                as D
import           Geometry                            as G
import           Geometry.Processes.ApollonianGasket
import           Numerics.Interpolation

import Test.TastyAll



tests :: TestTree
tests = testGroup "Apollonian gasket"
    [ correctGasket
    , spacedGasket
    , testGroup "Pretty bugs :-D"
        [ forgettingGen0
        , missingTheMinus
        ]
    ]

correctGasket :: TestTree
correctGasket = testVisual "The Gasket" 300 300 "docs/apollonian_gasket/classical_gasket" $ \_ -> do
    let gen0L = Circle (Vec2 100 100) 50
        gen0R = Circle (Vec2 200 100) 50
        gen0B = Circle (G.transform (rotateAround (Vec2 100 100) (deg 60)) (Vec2 200 100)) 50

        gasket = createGasket 0.5 gen0L gen0R gen0B

    for_ (zip [1..] gasket) $ \(i, Circle center r) -> cairoScope $ do
        setLineWidth 1
        setColor (rocket (linearInterpolate (1, fromIntegral (length gasket)) (1, 0) i))
        sketch (D.Circle center r)
        stroke

spacedGasket :: TestTree
spacedGasket = testVisual "Gasket with slightly spaced initial circles" 300 300 "docs/apollonian_gasket/spaced_gasket" $ \_ -> do
    let gen0L = Circle (Vec2 100 100) 42
        gen0R = Circle (Vec2 200 100) 42
        gen0B = Circle (G.transform (rotateAround (Vec2 100 100) (deg 60)) (Vec2 200 100)) 42

        gasket = createGasket 0.5 gen0L gen0R gen0B

    for_ (zip [1..] gasket) $ \(i, Circle center r) -> cairoScope $ do
        setLineWidth 1
        setColor (rocket (linearInterpolate (1, fromIntegral (length gasket)) (1, 0) i))
        sketch (D.Circle center r)
        stroke

forgettingGen0 :: TestTree
forgettingGen0 = testVisual "Forgetting to use the gen0 circles" 300 300 "docs/apollonian_gasket/forgetting_gen0" $ \_ -> do
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
            in sketch (D.Circle center (abs radius))

        apoCircles = concat
            [ []
            -- , [large]
            -- , [gen0L, gen0R, gen0B]
            , [gen1T, gen1L, gen1R]
            , recurse [(gen1T, gen1L, gen1R)]
            , recurse [(large, gen1T, gen1R)]
            , recurse [(large, gen1T, gen1L)]
            , recurse [(large, gen1L, gen1R)]
            ]

    for_ (zip [1..] apoCircles) $ \(i, apo) -> cairoScope $ do
        setLineWidth 1
        setColor (rocket (linearInterpolate (1, fromIntegral (length apoCircles)) (1, 0) i))
        apoCircleSketch apo
        stroke

missingTheMinus :: TestTree
missingTheMinus = testVisual "Missing that one minus" 300 300 "docs/apollonian_gasket/missing_the_minus" $ \_ -> do
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
            in sketch (D.Circle center (abs radius))

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
        setColor (rocket (linearInterpolate (1, fromIntegral (length apoCircles)) (1, 0) i))
        apoCircleSketch apo
        stroke
